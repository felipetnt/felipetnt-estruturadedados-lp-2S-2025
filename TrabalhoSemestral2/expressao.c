/* expressao.c
 *
 * Implementação compatível com expressao.h
 * - assinatura pública: int processarExpressao(const char *entrada, char **saida, float *valor, int *ehPos)
 * - retorno: *saida é malloc'd (caller deve free). Em erro retorna -1.
 * - compatível C90 (sem declarar variáveis dentro do for)
 * - não usa math.h nem -lm (funções aproximadas em float)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "expressao.h"

#define PI_F 3.14159265358979323846f
#define MAXTOKENS 1024
#define MAXTOKENLEN 128

/* protótipos internos */
char *normalizarInfixa(const char *expr);
char *infixaParaPosfixaInterna(const char *infixa_tokens); /* retorna malloc */
char *converterPosfixaParaInfixaInterna(const char *posfixa_tokens); /* retorna malloc */

int ehNumeroToken(const char *tok);
int ehOperadorToken(const char *tok);
int ehFuncaoToken(const char *tok);
int precedenciaToken(const char *tok);
int detectarPosfixa(const char *entrada);
int detectaPosFixa(const char *entrada);

float senoAprox(float graus);
float cossenoAprox(float graus);
float tangenteAprox(float graus);
float raizAprox(float x);
float lnAprox(float x);
float log10Aprox(float x);
float aplicarFuncaoUnaria(const char *func, float x);

/* ---------- pilha de floats ---------- */
typedef struct {
    float itens[256];
    int topo;
} PilhaFloat;

static void inicializarPilhaFloat(PilhaFloat *p) { p->topo = -1; }
static int pilhaFloatVazia(PilhaFloat *p) { return p->topo == -1; }
static int pilhaFloatCheia(PilhaFloat *p) { return p->topo == 255; }
static void empilharFloat(PilhaFloat *p, float v) { if (!pilhaFloatCheia(p)) p->itens[++(p->topo)] = v; }
static float desempilharFloat(PilhaFloat *p) { if (!pilhaFloatVazia(p)) return p->itens[(p->topo)--]; return 0.0f; }

/* ---------- utilitários de token ---------- */
int ehNumeroToken(const char *tok) {
    if (!tok || tok[0] == '\0') return 0;
    int i = 0;
    if (tok[0] == '-' && tok[1] != '\0') i = 1;
    int temPonto = 0;
    for (; tok[i] != '\0'; ++i) {
        if (tok[i] == '.') {
            if (temPonto) return 0;
            temPonto = 1;
        } else if (!isdigit((unsigned char)tok[i])) return 0;
    }
    return 1;
}

int ehOperadorToken(const char *tok) {
    if (!tok) return 0;
    if (strlen(tok) != 1) return 0;
    return (tok[0] == '+' || tok[0] == '-' || tok[0] == '*' ||
            tok[0] == '/' || tok[0] == '%' || tok[0] == '^');
}

int ehFuncaoToken(const char *tok) {
    if (!tok) return 0;
    if (strcmp(tok, "sen") == 0) return 1;
    if (strcmp(tok, "cos") == 0) return 1;
    if (strcmp(tok, "tg") == 0) return 1;
    if (strcmp(tok, "log") == 0) return 1;
    if (strcmp(tok, "log10") == 0) return 1;
    if (strcmp(tok, "raiz") == 0) return 1;
    if (strcmp(tok, "sqrt") == 0) return 1;
    return 0;
}

int precedenciaToken(const char *tok) {
    if (!tok) return 0;
    if (strcmp(tok, "+") == 0 || strcmp(tok, "-") == 0) return 1;
    if (strcmp(tok, "*") == 0 || strcmp(tok, "/") == 0 || strcmp(tok, "%") == 0) return 2;
    if (strcmp(tok, "^") == 0) return 3;
    return 0;
}

/* ---------- normalizar infixa -> tokens separados por espaço (malloc) ---------- */
char *normalizarInfixa(const char *expr) {
    if (!expr) return NULL;
    size_t n = strlen(expr);
    size_t cap = n * 3 + 16;
    char *tmp = (char*)malloc(cap);
    if (!tmp) return NULL;
    size_t w = 0;
    char last = '\0';
    size_t i;
    for (i = 0; i < n; ++i) {
        char c = expr[i];
        if (c == ' ' || c == '\t') continue;
        if (isalpha((unsigned char)c)) {
            char nome[MAXTOKENLEN];
            int p = 0;
            while (i < n && isalpha((unsigned char)expr[i]) && p + 1 < MAXTOKENLEN) {
                nome[p++] = expr[i++];
            }
            nome[p] = '\0';
            --i;
            if (w > 0 && tmp[w-1] != ' ') tmp[w++] = ' ';
            strcpy(tmp + w, nome);
            w += (int)strlen(nome);
            tmp[w++] = ' ';
            last = 'a';
            continue;
        }
        /* sinal unário '-' antes de número */
        if (c == '-' && i + 1 < n && (isdigit((unsigned char)expr[i+1]) || expr[i+1] == '.')) {
            if (last == '\0' || last == '(' || last == '+' || last == '-' ||
                last == '*' || last == '/' || last == '%' || last == '^') {
                tmp[w++] = '-';
                last = '-';
                continue;
            }
        }
        if (c == '(' || c == ')') {
            if (w > 0 && tmp[w-1] != ' ') tmp[w++] = ' ';
            tmp[w++] = c;
            tmp[w++] = ' ';
            last = c;
            continue;
        }
        if (strchr("+-*/%^", c)) {
            if (w > 0 && tmp[w-1] != ' ') tmp[w++] = ' ';
            tmp[w++] = c;
            tmp[w++] = ' ';
            last = c;
            continue;
        }
        /* dígitos e ponto */
        tmp[w++] = c;
        last = c;
    }
    tmp[w] = '\0';

    /* compacta espaços sequenciais */
    char *out = (char*)malloc(w + 1);
    if (!out) { free(tmp); return NULL; }
    size_t r = 0;
    int in_space = 0;
    for (i = 0; i < w; ++i) {
        char c = tmp[i];
        if (c == ' ') {
            if (!in_space && r > 0) { out[r++] = ' '; in_space = 1; }
        } else {
            out[r++] = c;
            in_space = 0;
        }
    }
    if (r > 0 && out[r-1] == ' ') r--;
    out[r] = '\0';
    free(tmp);
    return out;
}

/* ---------- detectar se string (já tokenizada com espaços) é posfixa (simulação) ---------- */
int detectarPosfixa(const char *entrada) {
    if (!entrada) return 0;
    char *copia = strdup(entrada);
    if (!copia) return 0;
    char *tok = strtok(copia, " ");
    int contador = 0;
    int valido = 1;
    while (tok && valido) {
        if (ehNumeroToken(tok)) {
            contador++;
        } else if (ehFuncaoToken(tok)) {
            if (contador < 1) { valido = 0; break; }
            /* função mantém contador (consome 1 empilha 1) */
        } else if (ehOperadorToken(tok)) {
            if (contador < 2) { valido = 0; break; }
            contador -= 1; /* consome 2 empilha 1 */
        } else {
            valido = 0;
            break;
        }
        tok = strtok(NULL, " ");
    }
    free(copia);
    return (valido && contador == 1) ? 1 : 0;
}

/* wrapper: detecta se entrada bruta é posfixa (sem parênteses, com espaços e válida) */
int detectaPosFixa(const char *entrada) {
    if (!entrada) return 0;
    if (strchr(entrada, '(') || strchr(entrada, ')')) return 0;
    if (strchr(entrada, ' ')) {
        if (detectarPosfixa(entrada)) return 1;
    }
    return 0;
}

/* ---------- INFIXA -> POSFIXA (shunting-yard).
   Retorna malloc'd string (tokens separados por espaço) ou NULL.
   Observação: quando empilhamos operadores/funções, duplicamos o token (strdup)
   para não referenciar buffer temporário. */
char *infixaParaPosfixaInterna(const char *infixa_raw) {
    if (!infixa_raw) return NULL;
    char *norm = normalizarInfixa(infixa_raw);
    if (!norm) return NULL;
    char *copia = strdup(norm);
    if (!copia) { free(norm); return NULL; }

    /* pilha de operadores: armazenamos strdup(token) */
    char **pilhaOp = (char**)malloc(sizeof(char*) * MAXTOKENS);
    if (!pilhaOp) { free(norm); free(copia); return NULL; }
    int topo = 0;

    /* saída dinâmica */
    size_t capOut = strlen(norm) * 2 + 32;
    char *saida = (char*)malloc(capOut);
    if (!saida) { free(copia); free(norm); free(pilhaOp); return NULL; }
    saida[0] = '\0';
    int j = 0;

    char *token = strtok(copia, " ");
    while (token) {
        if (ehNumeroToken(token)) {
            if (j > 0) { saida[j++] = ' '; saida[j] = '\0'; }
            strcpy(saida + j, token);
            j += (int)strlen(token);
        } else if (ehFuncaoToken(token)) {
            pilhaOp[topo++] = strdup(token); /* duplica */
            if (!pilhaOp[topo-1]) { /* falha malloc */
                /* cleanup */
                int k;
                for (k = topo-1; k >= 0; --k) free(pilhaOp[k]);
                free(pilhaOp); free(saida); free(norm); free(copia);
                return NULL;
            }
        } else if (strcmp(token, "(") == 0) {
            pilhaOp[topo++] = strdup(token);
            if (!pilhaOp[topo-1]) {
                int k;
                for (k = topo-1; k >= 0; --k) free(pilhaOp[k]);
                free(pilhaOp); free(saida); free(norm); free(copia);
                return NULL;
            }
        } else if (strcmp(token, ")") == 0) {
            while (topo > 0 && strcmp(pilhaOp[topo-1], "(") != 0) {
                if (j > 0) { saida[j++] = ' '; saida[j] = '\0'; }
                strcpy(saida + j, pilhaOp[topo-1]);
                j += (int)strlen(saida + j);
                free(pilhaOp[--topo]);
            }
            if (topo > 0 && strcmp(pilhaOp[topo-1], "(") == 0) {
                free(pilhaOp[--topo]); /* remove "(" */
            }
            if (topo > 0 && ehFuncaoToken(pilhaOp[topo-1])) {
                if (j > 0) { saida[j++] = ' '; saida[j] = '\0'; }
                strcpy(saida + j, pilhaOp[topo-1]);
                j += (int)strlen(saida + j);
                free(pilhaOp[--topo]);
            }
        } else if (ehOperadorToken(token)) {
            int prec = precedenciaToken(token);
            int right_assoc = (strcmp(token, "^") == 0);
            while (topo > 0 && strcmp(pilhaOp[topo-1], "(") != 0 && !ehFuncaoToken(pilhaOp[topo-1])) {
                if (ehOperadorToken(pilhaOp[topo-1])) {
                    int precTop = precedenciaToken(pilhaOp[topo-1]);
                    if (precTop > prec || (precTop == prec && !right_assoc)) {
                        if (j > 0) { saida[j++] = ' '; saida[j] = '\0'; }
                        strcpy(saida + j, pilhaOp[topo-1]);
                        j += (int)strlen(saida + j);
                        free(pilhaOp[--topo]);
                        continue;
                    }
                }
                break;
            }
            pilhaOp[topo++] = strdup(token);
            if (!pilhaOp[topo-1]) {
                int k;
                for (k = topo-1; k >= 0; --k) free(pilhaOp[k]);
                free(pilhaOp); free(saida); free(norm); free(copia);
                return NULL;
            }
        } else {
            /* desconhecido */
            int k;
            for (k = topo-1; k >= 0; --k) free(pilhaOp[k]);
            free(pilhaOp); free(saida); free(norm); free(copia);
            return NULL;
        }
        token = strtok(NULL, " ");
    }

    while (topo > 0) {
        if (j > 0) { saida[j++] = ' '; saida[j] = '\0'; }
        strcpy(saida + j, pilhaOp[topo-1]);
        j += (int)strlen(saida + j);
        free(pilhaOp[--topo]);
    }

    if (j == 0) saida[0] = '\0';
    free(pilhaOp);
    free(norm);
    free(copia);
    return saida;
}

/* ---------- POSFIXA -> INFIXA (construção com pilha de strings malloced) ---------- */
char *converterPosfixaParaInfixaInterna(const char *posfixa_raw) {
    if (!posfixa_raw) return NULL;
    char *copia = strdup(posfixa_raw);
    if (!copia) return NULL;

    char **pilha = (char**)malloc(sizeof(char*) * MAXTOKENS);
    if (!pilha) { free(copia); return NULL; }
    int topo = 0;

    char *token = strtok(copia, " ");
    while (token) {
        if (ehNumeroToken(token)) {
            pilha[topo] = (char*)malloc(strlen(token) + 1);
            if (!pilha[topo]) { /* cleanup */ while (topo>0) free(pilha[--topo]); free(pilha); free(copia); return NULL; }
            strcpy(pilha[topo++], token);
        } else if (ehFuncaoToken(token)) {
            if (topo < 1) { while (topo>0) free(pilha[--topo]); free(pilha); free(copia); return NULL; }
            char *arg = pilha[--topo];
            size_t len = strlen(token) + 1 + strlen(arg) + 3;
            char *novo = (char*)malloc(len);
            if (!novo) { free(arg); while (topo>0) free(pilha[--topo]); free(pilha); free(copia); return NULL; }
            sprintf(novo, "%s(%s)", token, arg);
            free(arg);
            pilha[topo++] = novo;
        } else if (ehOperadorToken(token)) {
            if (topo < 2) { while (topo>0) free(pilha[--topo]); free(pilha); free(copia); return NULL; }
            char *b = pilha[--topo];
            char *a = pilha[--topo];
            size_t len = strlen(a) + strlen(b) + strlen(token) + 5;
            char *novo = (char*)malloc(len);
            if (!novo) { free(a); free(b); while (topo>0) free(pilha[--topo]); free(pilha); free(copia); return NULL; }
            sprintf(novo, "(%s%s%s)", a, token, b);
            free(a); free(b);
            pilha[topo++] = novo;
        } else {
            while (topo>0) free(pilha[--topo]);
            free(pilha); free(copia); return NULL;
        }
        token = strtok(NULL, " ");
    }

    if (topo != 1) { while (topo>0) free(pilha[--topo]); free(pilha); free(copia); return NULL; }
    char *res = strdup(pilha[0]);
    free(pilha[0]);
    free(pilha);
    free(copia);

    /* remover parênteses externos correspondentes */
    size_t L = strlen(res);
    if (L >= 2 && res[0] == '(' && res[L-1] == ')') {
        int nivel = 0;
        int corresponde = 0;
        size_t idx;
        for (idx = 0; idx < L; ++idx) {
            if (res[idx] == '(') ++nivel;
            else if (res[idx] == ')') --nivel;
            if (nivel == 0 && idx < L - 1) { corresponde = 0; break; }
            if (idx == L-1 && nivel == 0) corresponde = 1;
        }
        if (corresponde) {
            char *sem = (char*)malloc(L - 1);
            if (sem) {
                strncpy(sem, res + 1, L - 2);
                sem[L-2] = '\0';
                free(res);
                res = sem;
            }
        }
    }

    return res;
}

/* ---------- funções trig / log / sqrt aproximadas (float) ---------- */
float grausParaRad(float g) { return g * PI_F / 180.0f; }

float senoAprox(float graus) {
    float x = grausParaRad(graus);
    float x2 = x * x;
    float termo = x;
    float soma = termo;
    termo *= -x2 / (2.0f * 3.0f); soma += termo;
    termo *= -x2 / (4.0f * 5.0f); soma += termo;
    termo *= -x2 / (6.0f * 7.0f); soma += termo;
    return soma;
}

float cossenoAprox(float graus) {
    float x = grausParaRad(graus);
    float x2 = x * x;
    float termo = 1.0f;
    float soma = termo;
    termo *= -x2 / (1.0f * 2.0f); soma += termo;
    termo *= -x2 / (3.0f * 4.0f); soma += termo;
    termo *= -x2 / (5.0f * 6.0f); soma += termo;
    return soma;
}

float tangenteAprox(float graus) {
    float c = cossenoAprox(graus);
    if (c == 0.0f) return 0.0f;
    return senoAprox(graus) / c;
}

float raizAprox(float x) {
    if (x <= 0.0f) return 0.0f;
    float r = x;
    int i;
    for (i = 0; i < 20; ++i) r = 0.5f * (r + x / r);
    return r;
}

float lnAprox(float x) {
    if (x <= 0.0f) return 0.0f;
    float y = (x - 1.0f) / (x + 1.0f);
    float y2 = y * y;
    float termo = y;
    float soma = termo;
    int n;
    for (n = 1; n < 8; ++n) {
        termo *= y2;
        soma += termo / (2.0f * n + 1.0f);
    }
    return 2.0f * soma;
}

float log10Aprox(float x) {
    const float LN10 = 2.302585092995f;
    return lnAprox(x) / LN10;
}

float aplicarFuncaoUnaria(const char *func, float x) {
    if (!func) return 0.0f;
    if (strcmp(func, "sen") == 0) return senoAprox(x);
    if (strcmp(func, "cos") == 0) return cossenoAprox(x);
    if (strcmp(func, "tg") == 0) return tangenteAprox(x);
    if (strcmp(func, "log") == 0) return log10Aprox(x);
    if (strcmp(func, "log10") == 0) return log10Aprox(x);
    if (strcmp(func, "raiz") == 0) return raizAprox(x);
    if (strcmp(func, "sqrt") == 0) return raizAprox(x);
    return 0.0f;
}

/* ---------- avaliação de posfixa (float) ---------- */
float getValorPosFixa(char *expr) {
    if (!expr) return 0.0f;
    PilhaFloat p;
    inicializarPilhaFloat(&p);

    char copia[2048];
    strncpy(copia, expr, sizeof(copia) - 1);
    copia[sizeof(copia)-1] = '\0';

    char *token = strtok(copia, " ");
    while (token) {
        if (ehNumeroToken(token)) {
            empilharFloat(&p, (float)atof(token));
        } else if (ehFuncaoToken(token)) {
            if (p.topo < 0) return 0.0f;
            float a = desempilharFloat(&p);
            empilharFloat(&p, aplicarFuncaoUnaria(token, a));
        } else if (ehOperadorToken(token)) {
            if (p.topo < 1) return 0.0f;
            float b = desempilharFloat(&p);
            float a = desempilharFloat(&p);
            float r = 0.0f;
            switch (token[0]) {
                case '+': r = a + b; break;
                case '-': r = a - b; break;
                case '*': r = a * b; break;
                case '/': r = (b != 0.0f) ? a / b : 0.0f; break;
                case '%': r = (float)((int)a % (int)b); break;
                case '^': {
                    int e = (int)b;
                    float acc = 1.0f;
                    int ii;
                    if (e >= 0) {
                        for (ii = 0; ii < e; ++ii) acc *= a;
                        r = acc;
                    } else {
                        for (ii = 0; ii < -e; ++ii) acc *= a;
                        if (acc != 0.0f) r = 1.0f / acc; else r = 0.0f;
                    }
                } break;
            }
            empilharFloat(&p, r);
        } else {
            return 0.0f;
        }
        token = strtok(NULL, " ");
    }

    if (p.topo < 0) return 0.0f;
    return desempilharFloat(&p);
}

/* ---------- funções públicas exigidas pelo header ---------- */
char *getFormaInFixa(char *Str) {
    return converterPosfixaParaInfixaInterna(Str);
}
char *infixaParaPosfixa(const char *infixa_raw) {
    return infixaParaPosfixaInterna(infixa_raw);
}

/* ---------- processarExpressao pública (aloca *saida com malloc) ---------- */
int processarExpressao(const char *entrada, char **saida, float *valor, int *ehPos) {
    if (!entrada || !saida || !valor || !ehPos) return -1;
    *saida = NULL;
    *valor = 0.0f;
    *ehPos = 0;

    /* detecta rápido: se contém parênteses -> infixa */
    int ehPosDirect = detectaPosFixa(entrada);

    char *norm = NULL;
    if (!ehPosDirect) {
        norm = normalizarInfixa(entrada);
        if (!norm) return -1;
    } else {
        norm = strdup(entrada);
        if (!norm) return -1;
    }

    /* confirmar após normalização */
    int ehPosFinal = detectarPosfixa(norm) ? 1 : 0;
    *ehPos = ehPosFinal;

    if (ehPosFinal) {
        /* entrada posfixa: converte para infixa legível e calcula */
        char *infixa = converterPosfixaParaInfixaInterna(norm);
        if (infixa) {
            *saida = infixa; /* caller deve free */
        } else {
            *saida = NULL;
        }
        *valor = getValorPosFixa(norm);
        free(norm);
        return 0;
    } else {
        /* entrada infixa: converte para posfixa e calcula */
        char *pos = infixaParaPosfixaInterna(norm);
        if (!pos) { free(norm); return -1; }
        *saida = pos; /* caller deve free */
        *valor = getValorPosFixa(pos);
        free(norm);
        return 0;
    }
}
