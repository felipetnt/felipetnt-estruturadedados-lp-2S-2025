#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "expressao.h"

char *converterInfixaParaPosfixa(char *infixa);

int main(){
    Expressao e;
    int opc;

    printf("1 - Converter pos-fixa para infixa e calcular\n");
    printf("2 - Converter infixa para pos-fixa e calcular\n");
    scanf("%d",&opc);
    getchar(); 

    if(opc==1){
        printf("Digite pos-fixa:\n");
        fgets(e.posFixa,512,stdin);
        e.posFixa[strcspn(e.posFixa,"\n")] = 0;

        char *inf = getFormaInFixa(e.posFixa);
        if(!inf){
            printf("Erro na conversao\n");
            return 1;
        }

        strcpy(e.inFixa, inf);
        free(inf);

        e.Valor = getValorPosFixa(e.posFixa);

        if(e.Valor != e.Valor){
            printf("Erro na avaliacao\n");
            return 1;
        }

        printf("Infixa: %s\nValor: %.6f\n", e.inFixa, e.Valor);
    }

    else if(opc==2){
        printf("Digite infixa:\n");
        fgets(e.inFixa,512,stdin);
        e.inFixa[strcspn(e.inFixa,"\n")] = 0;

        char *pos = converterInfixaParaPosfixa(e.inFixa);
        if(!pos){
            printf("Erro na conversao\n");
            return 1;
        }

        strncpy(e.posFixa, pos, 512);
        free(pos);

        e.Valor = getValorPosFixa(e.posFixa);

        if(e.Valor != e.Valor){
            printf("Erro na avaliacao\n");
            return 1;
        }

        printf("Pos-fixa: %s\nValor: %.6f\n", e.posFixa, e.Valor);
    }

    else {
        printf("Opcao invalida\n");
    }

    return 0;
}