#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "expressao.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

char *duplicarString(const char *texto){
    if(!texto) return NULL;
    size_t tamanho = strlen(texto);
    char *resultado = malloc(tamanho + 1);
    if(!resultado) return NULL;
    memcpy(resultado, texto, tamanho + 1);
    return resultado;
}

int ehOperador(char c){
    return c=='+' || c=='-' || c=='*' || c=='/' || c=='%' || c=='^';
}

int ehLetra(char c){
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

int ehDigitoOuPonto(char c){
    return (c >= '0' && c <= '9') || c == '.';
}

void substituirVirgulaPorPonto(char *texto){
    if(!texto) return;
    for(int i = 0; texto[i]; i++){
        if(texto[i] == ',') texto[i] = '.';
    }
}

char *lerToken(const char *texto, int *posicao){
    int tamanho = (int)strlen(texto);

    while(*posicao < tamanho && isspace((unsigned char)texto[*posicao]))
        (*posicao)++;

    if(*posicao >= tamanho) return NULL;

    if(ehDigitoOuPonto(texto[*posicao]) ||
       (texto[*posicao]=='-' && *posicao+1 < tamanho && ehDigitoOuPonto(texto[*posicao+1]))){
        
        int inicio = *posicao;
        if(texto[inicio] == '-') inicio++;

        while(inicio < tamanho && ehDigitoOuPonto(texto[inicio])) inicio++;

        int comprimento = inicio - *posicao;
        char *token = malloc(comprimento + 1);
        if(!token) return NULL;

        memcpy(token, texto + *posicao, comprimento);
        token[comprimento] = 0;

        *posicao = inicio;
        return token;
    }

    if(ehLetra(texto[*posicao])){
        int inicio = *posicao;
        while(inicio < tamanho && ehLetra(texto[inicio])) inicio++;

        int comprimento = inicio - *posicao;
        char *token = malloc(comprimento + 1);
        if(!token) return NULL;

        for(int i = 0; i < comprimento; i++)
            token[i] = (char)tolower((unsigned char)texto[*posicao + i]);

        token[comprimento] = 0;
        *posicao = inicio;
        return token;
    }

    if(ehOperador(texto[*posicao])){
        char temp[2];
        temp[0] = texto[*posicao];
        temp[1] = 0;
        (*posicao)++;
        return duplicarString(temp);
    }

    (*posicao)++;
    return NULL;
}

/* ---------- INFIXA → POSFIXA ----------- */
char *converterInfixaParaPosfixa(char *expressaoInfixa){
    if(!expressaoInfixa) return NULL;

    substituirVirgulaPorPonto(expressaoInfixa);

    char saida[1024];
    saida[0]=0;

    int n = (int)strlen(expressaoInfixa);

    int capacidadePilha = 64;
    char **pilhaOp = malloc(sizeof(char*) * capacidadePilha);
    if(!pilhaOp) return NULL;
    int topo = 0;

    int i = 0;
    while(i < n){
        if(isspace((unsigned char)expressaoInfixa[i])){ i++; continue; }

        if(ehDigitoOuPonto(expressaoInfixa[i]) || (expressaoInfixa[i]=='-' && i+1<n && ehDigitoOuPonto(expressaoInfixa[i+1]))){
            char num[128]; int p = 0;
            if(expressaoInfixa[i]=='-') num[p++] = expressaoInfixa[i++];
            while(i<n && ehDigitoOuPonto(expressaoInfixa[i])) num[p++] = expressaoInfixa[i++];
            num[p]=0;
            if(saida[0]) strcat(saida," ");
            strcat(saida,num);
            continue;
        }

        if(ehLetra(expressaoInfixa[i])){
            char nome[64]; int p = 0;
            while(i<n && ehLetra(expressaoInfixa[i])) nome[p++] = (char)tolower((unsigned char)expressaoInfixa[i++]);
            nome[p]=0;
            if(topo >= capacidadePilha){
                capacidadePilha *= 2;
                char **tmp = realloc(pilhaOp, sizeof(char*) * capacidadePilha);
                if(!tmp){ for(int k=0;k<topo;k++) free(pilhaOp[k]); free(pilhaOp); return NULL; }
                pilhaOp = tmp;
            }
            pilhaOp[topo++] = duplicarString(nome);
            continue;
        }

        if(expressaoInfixa[i] == '('){
            if(topo >= capacidadePilha){
                capacidadePilha *= 2;
                char **tmp = realloc(pilhaOp, sizeof(char*) * capacidadePilha);
                if(!tmp){ for(int k=0;k<topo;k++) free(pilhaOp[k]); free(pilhaOp); return NULL; }
                pilhaOp = tmp;
            }
            pilhaOp[topo++] = duplicarString("(");
            i++;
            continue;
        }

        if(expressaoInfixa[i] == ')'){
            while(topo > 0 && strcmp(pilhaOp[topo-1], "(") != 0){
                if(saida[0]) strcat(saida," ");
                strcat(saida, pilhaOp[topo-1]);
                free(pilhaOp[--topo]);
            }
            if(topo > 0 && strcmp(pilhaOp[topo-1], "(") == 0){
                free(pilhaOp[--topo]);
            }
            if(topo > 0 && ehLetra(pilhaOp[topo-1][0])){
                if(saida[0]) strcat(saida," ");
                strcat(saida, pilhaOp[topo-1]);
                free(pilhaOp[--topo]);
            }
            i++;
            continue;
        }

        if(ehOperador(expressaoInfixa[i])){
            char opAtual = expressaoInfixa[i];
            int prAtual = (opAtual=='+'||opAtual=='-')?1:((opAtual=='*'||opAtual=='/'||opAtual=='%')?2:3);

            while(topo > 0){
                char *topToken = pilhaOp[topo-1];
                if(strlen(topToken) == 1 && ehOperador(topToken[0]) && strcmp(topToken,"(")!=0){
                    char opTopo = topToken[0];
                    int prTopo = (opTopo=='+'||opTopo=='-')?1:((opTopo=='*'||opTopo=='/'||opTopo=='%')?2:3);
                    if( (prTopo > prAtual) || (prTopo == prAtual && opAtual != '^') ){
                        if(saida[0]) strcat(saida," ");
                        strcat(saida, topToken);
                        free(pilhaOp[--topo]);
                        continue;
                    }
                }
                break;
            }

            if(topo >= capacidadePilha){
                capacidadePilha *= 2;
                char **tmp = realloc(pilhaOp, sizeof(char*) * capacidadePilha);
                if(!tmp){ for(int k=0;k<topo;k++) free(pilhaOp[k]); free(pilhaOp); return NULL; }
                pilhaOp = tmp;
            }
            char opStr[2] = { opAtual, 0 };
            pilhaOp[topo++] = duplicarString(opStr);
            i++;
            continue;
        }

        i++;
    }

    while(topo > 0){
        if(strcmp(pilhaOp[topo-1], "(") == 0){
            free(pilhaOp[--topo]);
            continue;
        }
        if(saida[0]) strcat(saida," ");
        strcat(saida, pilhaOp[topo-1]);
        free(pilhaOp[--topo]);
    }

    free(pilhaOp);
    return duplicarString(saida);
}

/* ---------- AVALIAÇÃO POSFIXA ----------- */
float getValorPosFixa(char *expressaoPosfixa){
    if(!expressaoPosfixa) return NAN;

    substituirVirgulaPorPonto(expressaoPosfixa);

    int pos = 0;
    int capacidade = 64;
    int qtdTokens = 0;

    char **tokens = malloc(sizeof(char*) * capacidade);
    if(!tokens) return NAN;

    while(1){
        char *token = lerToken(expressaoPosfixa, &pos);
        if(!token) break;

        tokens[qtdTokens++] = token;

        if(qtdTokens >= capacidade){
            capacidade *= 2;
            tokens = realloc(tokens, sizeof(char*) * capacidade);
            if(!tokens) return NAN;
        }
    }

    double pilha[512];
    int topo = 0;

    for(int i = 0; i < qtdTokens; i++){
        char *token = tokens[i];

        if(ehDigitoOuPonto(token[0]) || (token[0]=='-' && strlen(token)>1)){
            pilha[topo++] = strtod(token, NULL);
            continue;
        }

        if(strlen(token)==1 && ehOperador(token[0])){
            if(topo < 2){
                for(int j=0;j<qtdTokens;j++) free(tokens[j]);
                free(tokens);
                return NAN;
            }

            double b = pilha[--topo];
            double a = pilha[--topo];
            double r = 0;

            switch(token[0]){
                case '+': r = a + b; break;
                case '-': r = a - b; break;
                case '*': r = a * b; break;
                case '/':
                    if(b == 0){
                        for(int j=0;j<qtdTokens;j++) free(tokens[j]);
                        free(tokens);
                        return NAN;
                    }
                    r = a / b;
                    break;
                case '%':
                    if(b == 0){
                        for(int j=0;j<qtdTokens;j++) free(tokens[j]);
                        free(tokens);
                        return NAN;
                    }
                    r = fmod(a, b);
                    break;
                case '^': r = pow(a, b); break;
            }

            pilha[topo++] = r;
            continue;
        }

        if(topo < 1){
            for(int j=0;j<qtdTokens;j++) free(tokens[j]);
            free(tokens);
            return NAN;
        }

        double a = pilha[--topo];
        double r = 0;
        char func[64];

        strncpy(func, token, 63);
        func[63] = 0;

        for(int k=0; func[k]; k++)
            func[k] = (char)tolower((unsigned char)func[k]);

        if(strcmp(func,"sen")==0)
            r = sin(a * M_PI / 180.0);
        else if(strcmp(func,"cos")==0)
            r = cos(a * M_PI / 180.0);
        else if(strcmp(func,"tg")==0){
            double c = cos(a * M_PI / 180.0);
            if(fabs(c) < 1e-12){
                for(int j=0;j<qtdTokens;j++) free(tokens[j]);
                free(tokens);
                return NAN;
            }
            r = tan(a * M_PI / 180.0);
        }
        else if(strcmp(func,"log")==0){
            if(a <= 0){
                for(int j=0;j<qtdTokens;j++) free(tokens[j]);
                free(tokens);
                return NAN;
            }
            r = log10(a);
        }
        else if(strcmp(func,"raiz")==0){
            if(a < 0){
                for(int j=0;j<qtdTokens;j++) free(tokens[j]);
                free(tokens);
                return NAN;
            }
            r = sqrt(a);
        }
        else {
            for(int j=0;j<qtdTokens;j++) free(tokens[j]);
            free(tokens);
            return NAN;
        }

        pilha[topo++] = r;
    }

    for(int j = 0; j < qtdTokens; j++) free(tokens[j]);
    free(tokens);

    if(topo != 1) return NAN;

    return (float) pilha[topo - 1];
}

/* ---------- POSFIXA → INFIXA ----------- */
char *getFormaInFixa(char *posfixa){
    if(!posfixa) return NULL;

    substituirVirgulaPorPonto(posfixa);

    int pos = 0, cap = 64, qtd = 0;
    char **tokens = malloc(sizeof(char*) * cap);
    if(!tokens) return NULL;

    while(1){
        char *t = lerToken(posfixa, &pos);
        if(!t) break;
        tokens[qtd++] = t;
        if(qtd >= cap){
            cap *= 2;
            tokens = realloc(tokens, sizeof(char*) * cap);
        }
    }

    typedef struct {
        char *str;
        int precedencia;
    } NoExpressao;

    NoExpressao *pilha = malloc(sizeof(NoExpressao) * 512);
    int topo = 0;

    for(int i = 0; i < qtd; i++){
        char *t = tokens[i];

        if(ehDigitoOuPonto(t[0]) || (t[0]=='-' && strlen(t)>1)){
            pilha[topo].str = duplicarString(t);
            pilha[topo].precedencia = 100;
            topo++;
            continue;
        }

        if(strlen(t)==1 && ehOperador(t[0])){
            NoExpressao direita = pilha[--topo];
            NoExpressao esquerda = pilha[--topo];

            int prioridade =
                (t[0]=='+'||t[0]=='-') ? 1 :
                (t[0]=='*'||t[0]=='/'||t[0]=='%') ? 2 : 3;

            int precisaParentesesEsq = esquerda.precedencia < prioridade;
            int precisaParentesesDir =
                direita.precedencia < prioridade ||
                (direita.precedencia == prioridade && t[0] != '^');

            size_t tamanho =
                strlen(esquerda.str) + strlen(direita.str) + 8 +
                (precisaParentesesEsq?2:0) + (precisaParentesesDir?2:0);

            char *nova = malloc(tamanho);
            nova[0] = 0;

            if(precisaParentesesEsq) strcat(nova,"(");
            strcat(nova, esquerda.str);
            if(precisaParentesesEsq) strcat(nova,")");

            size_t posNova = strlen(nova);
            nova[posNova] = t[0];
            nova[posNova+1] = 0;

            if(precisaParentesesDir) strcat(nova,"(");
            strcat(nova, direita.str);
            if(precisaParentesesDir) strcat(nova,")");

            free(esquerda.str);
            free(direita.str);

            pilha[topo].str = nova;
            pilha[topo].precedencia = prioridade;
            topo++;
            continue;
        }

        NoExpressao arg = pilha[--topo];

        size_t tamanho = strlen(t) + strlen(arg.str) + 4;
        char *nova = malloc(tamanho);

        sprintf(nova, "%s(%s)", t, arg.str);

        free(arg.str);

        pilha[topo].str = nova;
        pilha[topo].precedencia = 4;
        topo++;
    }

    char *resultado = pilha[0].str;
    free(pilha);

    for(int j = 0; j < qtd; j++)
        free(tokens[j]);
    free(tokens);

    return resultado;
}