#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "calculadora.h"

#define M_PI 3.14159265358979323846

// ---------- Funções auxiliares internas ----------

char *duplicarString(const char *origem){
    if(!origem) return NULL;
    size_t tamanho = strlen(origem);
    char *nova = (char*)malloc(tamanho + 1);
    if(!nova) return NULL;
    memcpy(nova, origem, tamanho + 1);
    return nova;
}

int tokenEhNumero(const char *token){
    if(!token || *token=='\0') return 0;

    char buffer[256];
    strncpy(buffer, token, 255);
    buffer[255] = 0;

    // Troca vírgula por ponto
    int i;
    for(i=0; buffer[i]; i++)
        if(buffer[i] == ',')
            buffer[i] = '.';

    char *fim;
    strtod(buffer, &fim);

    return (fim != buffer && *fim=='\0');
}

int tokenEhOperadorBinario(const char *token){
    return strlen(token)==1 && 
        (token[0]=='+'||token[0]=='-'||token[0]=='*'||
         token[0]=='/'||token[0]=='%'||token[0]=='^');
}

void converterParaMinusculo(char *s){
    int i;
    for(i=0; s[i]; i++)
        s[i] = (char)tolower((unsigned char)s[i]);
}

int tokenEhFuncaoUnaria(const char *token){
    char buffer[16];
    strncpy(buffer, token, 15);
    buffer[15] = 0;
    converterParaMinusculo(buffer);

    return !strcmp(buffer,"sen") || !strcmp(buffer,"cos") ||
           !strcmp(buffer,"tg")  || !strcmp(buffer,"log") ||
           !strcmp(buffer,"raiz");
}

int aplicarOperadorBinario(char operador, double a, double b, double *resultado){
    switch(operador){
        case '+': *resultado = a+b; return 1;
        case '-': *resultado = a-b; return 1;
        case '*': *resultado = a*b; return 1;
        case '/': if(b == 0) return 0; *resultado = a/b; return 1;
        case '%': if(b == 0) return 0; *resultado = fmod(a,b); return 1;
        case '^': *resultado = pow(a,b); return 1;
    }
    return 0;
}

int aplicarFuncaoUnaria(const char *funcao, double valor, double *resultado){
    char buffer[16];
    strncpy(buffer, funcao, 15);
    buffer[15] = 0;
    converterParaMinusculo(buffer);

    if(!strcmp(buffer,"sen")){
        *resultado = sin(valor*M_PI/180.0);
        return 1;
    }
    if(!strcmp(buffer,"cos")){
        *resultado = cos(valor*M_PI/180.0);
        return 1;
    }
    if(!strcmp(buffer,"tg")){
        double c = cos(valor*M_PI/180.0);
        if(fabs(c) < 1e-12) return 0;
        *resultado = tan(valor*M_PI/180.0);
        return 1;
    }
    if(!strcmp(buffer,"log")){
        if(valor <= 0) return 0;
        *resultado = log10(valor);
        return 1;
    }
    if(!strcmp(buffer,"raiz")){
        if(valor < 0) return 0;
        *resultado = sqrt(valor);
        return 1;
    }

    return 0;
}

// -----------------------------------------------------------
//               CONVERSÃO: POSFIXA → INFIXA
// -----------------------------------------------------------

char * getFormaInFixa(char *expressaoPosfixa){
    if(!expressaoPosfixa) return NULL;

    char *copia = duplicarString(expressaoPosfixa);
    if(!copia) return NULL;

    char **pilhaStrings = malloc(sizeof(char*) * 64);
    if(!pilhaStrings){ free(copia); return NULL; }

    int topo = 0;
    int capacidade = 64;
    int erro = 0;

    char *token = strtok(copia, " \t\r\n");

    while(token && !erro){

        if(tokenEhNumero(token)){
            char *numero = duplicarString(token);
            if(!numero){ erro = 1; break; }

            if(topo >= capacidade){
                capacidade *= 2;
                char **novaPilha = realloc(pilhaStrings, sizeof(char*)*capacidade);
                if(!novaPilha){ free(numero); erro = 1; break; }
                pilhaStrings = novaPilha;
            }

            pilhaStrings[topo++] = numero;
        }

        else if(tokenEhOperadorBinario(token)){
            if(topo < 2){ erro = 1; break; }

            char *direito = pilhaStrings[--topo];
            char *esquerdo = pilhaStrings[--topo];

            size_t tamanhoNovo = strlen(esquerdo) + strlen(direito) + 4;
            char *novaExpressao = malloc(tamanhoNovo + 1);
            if(!novaExpressao){ free(esquerdo); free(direito); erro=1; break; }

            sprintf(novaExpressao, "(%s%c%s)", esquerdo, token[0], direito);

            free(esquerdo);
            free(direito);

            pilhaStrings[topo++] = novaExpressao;
        }

        else if(tokenEhFuncaoUnaria(token)){
            if(topo < 1){ erro = 1; break; }

            char *argumento = pilhaStrings[--topo];

            char funcaoLower[32];
            strncpy(funcaoLower, token, 31);
            funcaoLower[31] = 0;
            converterParaMinusculo(funcaoLower);

            size_t tamanhoNovo = strlen(funcaoLower) + strlen(argumento) + 3;
            char *novaExpressao = malloc(tamanhoNovo);
            if(!novaExpressao){ free(argumento); erro = 1; break; }

            sprintf(novaExpressao, "%s(%s)", funcaoLower, argumento);

            free(argumento);

            pilhaStrings[topo++] = novaExpressao;
        }

        else {
            erro = 1;
        }

        token = strtok(NULL, " \t\r\n");
    }

    free(copia);
    
    if(erro || topo != 1){
        for(int i=0; i<topo; i++)
            free(pilhaStrings[i]);
        free(pilhaStrings);
        return NULL;
    }

    char *resultado = pilhaStrings[0];
    free(pilhaStrings);
    return resultado;
}

// -----------------------------------------------------------
//               AVALIAÇÃO DE EXPRESSÃO POS-FIXA
// -----------------------------------------------------------

float getValorPosFixa(char *expressaoPosfixa){
    if(!expressaoPosfixa) return NAN;

    char *copia = duplicarString(expressaoPosfixa);
    if(!copia) return NAN;

    double *pilhaValores = malloc(sizeof(double) * 64);
    if(!pilhaValores){ free(copia); return NAN; }

    int topo = 0;
    int capacidade = 64;
    int erro = 0;

    char *token = strtok(copia, " \t\r\n");

    while(token && !erro){

        char buffer[256];
        strncpy(buffer, token, 255);
        buffer[255] = 0;

        for(int i=0; buffer[i]; i++)
            if(buffer[i] == ',')
                buffer[i] = '.';

        char *fim;
        double numero = strtod(buffer, &fim);

        if(fim != buffer && *fim == '\0'){
            if(topo >= capacidade){
                capacidade *= 2;
                double *nova = realloc(pilhaValores, sizeof(double)*capacidade);
                if(!nova){ erro = 1; break; }
                pilhaValores = nova;
            }
            pilhaValores[topo++] = numero;
        }

        else if(tokenEhOperadorBinario(token)){
            if(topo < 2){ erro = 1; break; }

            double b = pilhaValores[--topo];
            double a = pilhaValores[--topo];
            double r;

            if(!aplicarOperadorBinario(token[0], a, b, &r)){ erro = 1; break; }

            pilhaValores[topo++] = r;
        }

        else if(tokenEhFuncaoUnaria(token)){
            if(topo < 1){ erro = 1; break; }

            double a = pilhaValores[--topo];
            double r;

            if(!aplicarFuncaoUnaria(token, a, &r)){ erro = 1; break; }

            pilhaValores[topo++] = r;
        }

        else {
            erro = 1;
        }

        token = strtok(NULL, " \t\r\n");
    }

    free(copia);

    if(erro || topo != 1){
        free(pilhaValores);
        return NAN;
    }

    float resultado = (float)pilhaValores[0];
    free(pilhaValores);
    return resultado;
}
