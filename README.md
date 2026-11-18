# Análise de Componentes Principais para Séries Temporais: Uma Aplicação da Teoria Assintótica à Inflação de Preços de Alimentos no Brasil

Trabalho apresentado na $70^a$ Reunião Anual da Região Brasileira da Sociedade Internacional de Biometria (RBras), Campinas, SP.

A Análise de Componentes Principais (ACP) é uma técnica multivariada amplamente utilizada
para a redução de dimensionalidade por meio de combinações lineares de variáveis. A teoria
clássica da ACP pressupõe que as observações sejam independentes. No entanto, ao tratar de
séries temporais, frequentemente, os dados apresentam forte dependência temporal e correlação
cruzada. Ignorar essa estrutura de dependência resulta na subestimação dos erros padrão dos
pesos (loadings) das componentes, levando a inferências incorretas. Neste contexto, o estudo
tem como objetivo aplicar a teoria assintótica da ACP para séries temporais a dados de inflação
de preços de alimentos no Brasil, avaliando e comparando seu desempenho em relação à
abordagem clássica da ACP. Foram utilizadas 16 séries temporais mensais, no período de
janeiro de 2000 a julho de 2026, representando o histórico de inflação de diferentes grupos
alimentícios no país. A metodologia consistiu na verificação inicial de estacionariedade das
séries por meio de testes de raiz unitária, seguida da aplicação da ACP. Para a inferência
estatística dos pesos das componentes, utilizou-se o método de reamostragem Moving Block
Bootstrap (MBB), adequado para contornar limitações de processos com dependência e
distribuição não-gaussiana. Os resultados indicam que a abordagem clássica da ACP gera falsos
positivos devido ao ruído temporal autocorrelacionado não capturado, agrupando
equivocadamente alimentos de cadeias produtivas distintas, o que cria a falsa impressão de que
seus preços oscilam em conjunto. Em contrapartida, a abordagem assintótica via MBB filtra
adequadamente esses ruídos e gera componentes mais esparsas, isolando adequadamente os
grupos alimentícios. As conclusões visam contribuir para a identificação correta de fatores de
pressão inflacionária, auxiliando de forma mais adequada o planejamento de políticas públicas
e futuras investigações na área econômica e agrícola.
