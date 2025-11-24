#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "expressao.h"

// Protótipo real da função implementada no expressao.c
int processarExpressao(const char *entrada, char **saida, float *valor, int *ehPos);

void testar(const char *expr) {
    char *saida = NULL;   // agora recebe malloc do processarExpressao
    float valor = 0.0f;
    int ehPos = 0;

    printf("\n===============================\n");
    printf("Expressao de entrada: %s\n", expr);

    if (processarExpressao(expr, &saida, &valor, &ehPos) == 0) {

        if (ehPos) {
            printf("Tipo detectado: POS-FIXA\n");
            printf("Convertida para INFIXA: %s\n", saida);
        } else {
            printf("Tipo detectado: INFIXA\n");
            printf("Convertida para POS-FIXA: %s\n", saida);
        }

        printf("Valor calculado: %.6f\n", valor);
    } else {
        printf("ERRO ao processar expressao!\n");
    }

    if (saida) free(saida);  // libera o malloc feito dentro do expressao.c
}

int main() {

    // ======= TESTES QUE VOCÊ PEDIU =======

    testar("3 4 + 5 *");
    testar("(3 + 4) * 5");

    testar("7 2 * 4 +");
    testar("7 * 2 + 4");

    testar("8 5 2 4 + * +");
    testar("8 + (5 * (2 + 4))");

    testar("6 2 / 3 + 4 *");
    testar("(6 / 2 + 3) * 4");

    testar("9 5 2 8 * 4 + * +");
    testar("9 + (5 * (2 * 8 + 4))");

    testar("2 3 + log 5 /");
    testar("log(2 + 3) / 5");

    testar("10 log 3 ^ 2 +");
    testar("(log(10)) ^ 3 + 2");

    testar("45 60 + 30 cos *");
    testar("(45 + 60) * cos(30)");

    testar("0.5 45 sen 2 ^ +");
    testar("sen(45) ^ 2 + 0.5");

    return 0;
}
