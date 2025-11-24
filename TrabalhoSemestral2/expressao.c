/* expressao.c - implementação compatível com expressao.h (sem -lm) */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "expressao.h"

#define PI_F 3.14159265358979323846f
#define MAXTOKENS 1024
#define MAXTOKENLEN 128

char *normalizarInfixa(const char *expr);
char *infixaParaPosfixaInterna(const char *infixa_tokens); /* retorna malloc */
char *converterPosfixaParaInfixaInterna(const char *posfixa_tokens); /* retorna malloc */

int ehNumeroToken(const char *tok);
int ehOperadorToken(const char *tok);
int ehFuncaoToken(const char *tok);
int precedenciaToken(const char *tok);
int detectarPosfixa(const char *entrada);
int detectaPosFixa(const char *entrada);

static char *minha_strdup(const char *s);

float senoAprox(float graus);
float cossenoAprox(float graus);
float tangenteAprox(float graus);
float raizAprox(float x);
float lnAprox(float x);
float log10Aprox(float x);
float aplicarFuncaoUnaria(const char *func, float x);

static int tem_par_externa(const char *s) {
    if (!s) return 0;
    if (s[0] != '(') return 0;
    int nivel = 0;
    size_t i;
    size_t L = strlen(s);
    for (i = 0; i < L; ++i) {
        if (s[i] == '(') ++nivel;
        else if (s[i] == ')') {
            --nivel;
            if (nivel == 0 && i < L - 1) return 0;
        }
    }
    return (nivel == 0) ? 1 : 0;
}

static char top_level_high_op(const char *t) {
    if (!t) return 0;
    int n = 0; size_t j;
    for (j = 0; t[j]; ++j) {
        if (t[j] == '(') ++n;
        else if (t[j] == ')') --n;
        else if (n == 0) {
            if (t[j] == '*' || t[j] == '/' || t[j] == '%' || t[j] == '^') return t[j];
        }
    }
    return 0;
}

static int left_token_is_func_or_paren(const char *s) {
    if (!s) return 0;
    size_t i = 0;
    while (s[i] && isspace((unsigned char)s[i])) i++;
    if (!s[i]) return 0;
    if (s[i] == '(') return 1;
    if (isalpha((unsigned char)s[i])) return 1;
    return 0;
}

static char *ajustar_parenteses_root(char *s) {
    if (!s) return s;
    size_t L = strlen(s);
    int nivel = 0;
    int pos = -1;
    size_t i;
    /* encontra o último '+' ou '-' em nível 0 (divide principal) */
    for (i = 0; i < L; ++i) {
        char c = s[i];
        if (c == '(') ++nivel;
        else if (c == ')') --nivel;
        else if (nivel == 0 && (c == '+' || c == '-')) {
            pos = (int)i; /* manter última ocorrência */
        }
    }
    if (pos < 0) return s;
    /* separa em dois lados */
    char *esq = (char*)malloc(pos + 1);
    if (!esq) return s;
    memcpy(esq, s, pos);
    esq[pos] = '\0';
    char *dir = minha_strdup(s + pos + 1);
    if (!dir) { free(esq); return s; }

    char opLeft = top_level_high_op(esq);
    char opRight = top_level_high_op(dir);

    int need_left = (opLeft != 0) && !tem_par_externa(esq);
    int need_right = (opRight != 0) && !tem_par_externa(dir);

    if (opLeft == '^' && left_token_is_func_or_paren(esq)) need_left = 0;
    if (opRight == '^' && left_token_is_func_or_paren(dir)) need_right = 0;

    if (!need_left && !need_right) { free(esq); free(dir); return s; }

    /* monta nova string */
    size_t newlen = strlen(esq) + strlen(dir) + 3 + (need_left?2:0) + (need_right?2:0);
    char *novo = (char*)malloc(newlen + 1);
    if (!novo) { free(esq); free(dir); return s; }
    novo[0] = '\0';
    if (need_left) { strcat(novo, "("); strcat(novo, esq); strcat(novo, ")"); }
    else strcat(novo, esq);
    {
        size_t p = strlen(novo);
        novo[p] = s[pos];
        novo[p+1] = '\0';
    }
    if (need_right) { strcat(novo, "("); strcat(novo, dir); strcat(novo, ")"); }
    else strcat(novo, dir);

    free(esq); free(dir); free(s);
    return novo;
}

typedef struct {
    float itens[256];
    int topo;
} PilhaFloat;

static void inicializarPilhaFloat(PilhaFloat *p){p->topo=-1;} static int pilhaFloatVazia(PilhaFloat *p){return p->topo==-1;} static int pilhaFloatCheia(PilhaFloat *p){return p->topo==255;} static void empilharFloat(PilhaFloat *p,float v){if(!pilhaFloatCheia(p))p->itens[++(p->topo)]=v;} static float desempilharFloat(PilhaFloat *p){if(!pilhaFloatVazia(p))return p->itens[(p->topo)--];return 0.0f;}

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

int detectarPosfixa(const char *entrada) {
    if (!entrada) return 0;
    char *copia = minha_strdup(entrada);
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

int detectaPosFixa(const char *entrada) {
    if (!entrada) return 0;
    if (strchr(entrada, '(') || strchr(entrada, ')')) return 0;
    if (strchr(entrada, ' ')) {
        if (detectarPosfixa(entrada)) return 1;
    }
    return 0;
}

char *infixaParaPosfixaInterna(const char *infixa_raw) {
    if (!infixa_raw) return NULL;
    char *norm = normalizarInfixa(infixa_raw);
    if (!norm) return NULL;
    char *copia = minha_strdup(norm);
    if (!copia) { free(norm); return NULL; }

    /* pilha de operadores */
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
            pilhaOp[topo++] = minha_strdup(token); /* duplica */
            if (!pilhaOp[topo-1]) { /* falha malloc */
                /* cleanup */
                int k;
                for (k = topo-1; k >= 0; --k) free(pilhaOp[k]);
                free(pilhaOp); free(saida); free(norm); free(copia);
                return NULL;
            }
        } else if (strcmp(token, "(") == 0) {
            pilhaOp[topo++] = minha_strdup(token);
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
            pilhaOp[topo++] = minha_strdup(token);
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

char *converterPosfixaParaInfixaInterna(const char *posfixa_raw) {
    if (!posfixa_raw) return NULL;
    char *copia = minha_strdup(posfixa_raw);
    if (!copia) return NULL;
    /* Detecta parênteses externos envolvendo toda a entrada posfixa */
    int wrap_final = 0;
    {
        size_t L = strlen(copia);
        size_t s = 0;
        while (s < L && isspace((unsigned char)copia[s])) s++;
        size_t e = L;
        while (e > s && isspace((unsigned char)copia[e-1])) e--;
        if (e - s >= 2 && copia[s] == '(' && copia[e-1] == ')') {
            /* copia substring [s+1, e-1) */
            size_t newlen = e - s - 1;
            char *tmp = (char*)malloc(newlen + 1);
            if (tmp) {
                memcpy(tmp, copia + s + 1, newlen - 1);
                tmp[newlen - 1] = '\0';
                free(copia);
                copia = tmp;
                wrap_final = 1;
            }
        }
    }

    typedef struct {
        char *str;
        int prec;
    } NoExpr;

    NoExpr *pilha = (NoExpr*)malloc(sizeof(NoExpr) * MAXTOKENS);
    if (!pilha) { free(copia); return NULL; }
    int topo = 0;

    char *token = strtok(copia, " ");
    while (token) {
        if (ehNumeroToken(token)) {
            pilha[topo].str = minha_strdup(token);
            if (!pilha[topo].str) { while (topo>0) free(pilha[--topo].str); free(pilha); free(copia); return NULL; }
            pilha[topo].prec = 100;
            topo++;
        } else if (ehFuncaoToken(token)) {
            if (topo < 1) { while (topo>0) free(pilha[--topo].str); free(pilha); free(copia); return NULL; }
            char *arg = pilha[--topo].str;
            size_t len = strlen(token) + 1 + strlen(arg) + 3;
            char *novo = (char*)malloc(len);
            if (!novo) { free(arg); while (topo>0) free(pilha[--topo].str); free(pilha); free(copia); return NULL; }
            sprintf(novo, "%s(%s)", token, arg);
            free(arg);
            pilha[topo].str = novo;
            pilha[topo].prec = 4;
            topo++;
        } else if (ehOperadorToken(token)) {
            if (topo < 2) { while (topo>0) free(pilha[--topo].str); free(pilha); free(copia); return NULL; }
            char *b = pilha[--topo].str;
            int precB = pilha[topo].prec;
            char *a = pilha[--topo].str;
            int precA = pilha[topo].prec;

            int prioridade = (token[0] == '+' || token[0] == '-') ? 1 : ((token[0] == '*' || token[0] == '/' || token[0] == '%') ? 2 : 3);
            int is_right_assoc = (token[0] == '^');

            int precisaParEsq = (precA < prioridade) || (precA == prioridade && is_right_assoc);
            int precisaParDir = (precB < prioridade) || (precB == prioridade && !is_right_assoc);

            

            size_t len = strlen(a) + strlen(b) + 5 + (precisaParEsq?2:0) + (precisaParDir?2:0);
            char *novo = (char*)malloc(len);
            if (!novo) { free(a); free(b); while (topo>0) free(pilha[--topo].str); free(pilha); free(copia); return NULL; }
            novo[0] = '\0';
            if (precisaParEsq) {
                if (tem_par_externa(a)) {
                    strcat(novo, a);
                } else {
                    strcat(novo, "(");
                    strcat(novo, a);
                    strcat(novo, ")");
                }
            } else {
                strcat(novo, a);
            }
            {
                size_t p = strlen(novo);
                novo[p] = token[0];
                novo[p+1] = '\0';
            }
            if (precisaParDir) {
                if (tem_par_externa(b)) {
                    strcat(novo, b);
                } else {
                    strcat(novo, "(");
                    strcat(novo, b);
                    strcat(novo, ")");
                }
            } else {
                strcat(novo, b);
            }

            free(a); free(b);
            pilha[topo].str = novo;
            pilha[topo].prec = prioridade;
            topo++;
        } else {
            while (topo>0) free(pilha[--topo].str);
            free(pilha); free(copia); return NULL;
        }
        token = strtok(NULL, " ");
    }

    if (topo != 1) { while (topo>0) free(pilha[--topo].str); free(pilha); free(copia); return NULL; }
    char *res = minha_strdup(pilha[0].str);
    free(pilha[0].str);
    free(pilha);
    free(copia);

    if (res && wrap_final) {
        size_t lr = strlen(res);
        char *aux = (char*)malloc(lr + 3);
        if (aux) {
            aux[0] = '(';
            memcpy(aux + 1, res, lr);
            aux[lr+1] = ')';
            aux[lr+2] = '\0';
            free(res);
            res = aux;
        }
    }

     res = ajustar_parenteses_root(res);

    return res;
}

float grausParaRad(float g){return g*PI_F/180.0f;}

/* Usar math.h para resultados mais precisos; entrada em graus para trig */
float senoAprox(float graus){return sinf(grausParaRad(graus));}
float cossenoAprox(float graus){return cosf(grausParaRad(graus));}
float tangenteAprox(float graus){return tanf(grausParaRad(graus));}
float raizAprox(float x){return (x<=0.0f)?0.0f:sqrtf(x);} 
float lnAprox(float x){return (x<=0.0f)?0.0f:logf(x);} 
float log10Aprox(float x){return (x<=0.0f)?0.0f:log10f(x);} 
float aplicarFuncaoUnaria(const char *func, float x){
    if (!func) return 0.0f;
    if (strcmp(func,"sen")==0) return senoAprox(x);
    if (strcmp(func,"cos")==0) return cossenoAprox(x);
    if (strcmp(func,"tg")==0) return tangenteAprox(x);
    if (strcmp(func,"log")==0) return log10Aprox(x);
    if (strcmp(func,"log10")==0) return log10Aprox(x);
    if (strcmp(func,"raiz")==0) return raizAprox(x);
    if (strcmp(func,"sqrt")==0) return raizAprox(x);
    return 0.0f;
}
float getValorPosFixa(char *expr){
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
char *getFormaInFixa(char *Str){
    return converterPosfixaParaInfixaInterna(Str);
}char *infixaParaPosfixa(const char *infixa_raw){return infixaParaPosfixaInterna(infixa_raw);}int processarExpressao(const char *entrada,char **saida,float *valor,int *ehPos){
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
        norm = minha_strdup(entrada);
        if (!norm) return -1;
    }

    /* confirmar após normalização */
    int ehPosFinal = detectarPosfixa(norm) ? 1 : 0;
    *ehPos = ehPosFinal;

    if(ehPosFinal){
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
/* minha_strdup: implementação local de strdup */
static char *minha_strdup(const char *s){if(!s)return NULL;size_t n=strlen(s);char*r=(char*)malloc(n+1);if(!r)return NULL;memcpy(r,s,n+1);return r;}