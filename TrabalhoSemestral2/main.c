#include <stdio.h>
#include <string.h>
#include "calculadora.h"

int main() {
    char entrada[512];
    char *infixa;
    float valor;

    printf("Digite a expressao pos-fixa: ");
    fgets(entrada, sizeof(entrada), stdin);

    entrada[strcspn(entrada, "\n")] = 0;

    infixa = getFormaInFixa(entrada);
    if (infixa == NULL) {
        printf("Erro ao converter para infixa.\n");
        return 1;
    }

    valor = getValorPosFixa(entrada);

    printf("Forma infixa: %s\n", infixa);
    printf("Valor: %.2f\n", valor);

    return 0;
}