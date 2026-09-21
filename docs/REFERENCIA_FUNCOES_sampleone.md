# Referência de Funções — pacote `sampleone`

Tabela completa das 44 funções exportadas + os métodos `print()`/`summary()`, 
organizadas por capítulo. Todas as funções aceitam argumentos excedentes sem erro 
(uma variável que não é usada pela fórmula selecionada é simplesmente ignorada), 
e nenhuma depende de pacotes externos ao R base (`stats`/`utils`, já inclusos na instalação padrão).

Toda função retorna um objeto `sampling_result`, que pode ser encadeado com o operador pipe nativo do R (`|>`) para `print()` ou `summary()`, ex.: `aas_tamanho_media(sigma = 7, d = 2) |> summary()`.


## Cap. 2 - Conceitos base

### `pop_total()`

- **Inputs:** x: vetor numerico (populacao completa)
- **Descrição:** Total populacional (soma de todos os valores)
- **Como chamar:**
```r
pop_total(c(1,2,3,4,5))
# ou: c(1,2,3,4,5) |> pop_total()
```
- **Pacotes necessários:** Nenhum

### `pop_media()`

- **Inputs:** x: vetor numerico (populacao completa)
- **Descrição:** Media e variancia populacional (divisor N)
- **Como chamar:**
```r
pop_media(c(44,45,48,42,46))
```
- **Pacotes necessários:** Nenhum

### `pop_proporcao()`

- **Inputs:** x: vetor 0/1 ou logico (populacao completa)
- **Descrição:** Proporcao populacional e Var(P)=P(1-P)
- **Como chamar:**
```r
pop_proporcao(c(1,0,0,1,1))
```
- **Pacotes necessários:** Nenhum

### `amostra_estimadores()`

- **Inputs:** x: vetor numerico (dados amostrais)
- **Descrição:** Media amostral, variancia amostral (divisor n-1) e, se dados 0/1, proporcao amostral -- tudo de uma vez
- **Como chamar:**
```r
amostra_estimadores(c(12,30,18))
```
- **Pacotes necessários:** Nenhum


## Cap. 3 - AAS

### `aas_estima_media()`

- **Inputs:** x: vetor amostral | N: tam. populacao (opcional) | reposicao: 'sem'/'com' | conf: nivel confianca
- **Descrição:** Estima a media populacional (AAS com ou sem reposicao), variancia, CV e IC
- **Como chamar:**
```r
aas_estima_media(x, N = 300, reposicao = 'sem') |> summary()
```
- **Pacotes necessários:** Nenhum

### `aas_estima_total()`

- **Inputs:** x: vetor amostral | N: tam. populacao (obrigatorio) | reposicao | conf
- **Descrição:** Estima o total populacional a partir de uma amostra AAS
- **Como chamar:**
```r
aas_estima_total(x, N = 300)
```
- **Pacotes necessários:** Nenhum

### `aas_estima_proporcao()`

- **Inputs:** x: vetor 0/1 (amostra) | N (opcional) | reposicao | conf
- **Descrição:** Estima uma proporcao populacional (AAS com ou sem reposicao)
- **Como chamar:**
```r
aas_estima_proporcao(x, N = 500)
```
- **Pacotes necessários:** Nenhum

### `aas_tamanho_media()`

- **Inputs:** sigma2 ou sigma | d: erro tolerado | conf | N (opcional) | perda (opcional) | arredondar
- **Descrição:** Tamanho de amostra para estimar uma media (formula automatica conforme N informado ou nao)
- **Como chamar:**
```r
aas_tamanho_media(sigma = 7, d = 2, conf = 0.95)
```
- **Pacotes necessários:** Nenhum

### `aas_tamanho_proporcao()`

- **Inputs:** p (opcional, default conservador 0.5) | d | conf | N (opcional) | perda | arredondar
- **Descrição:** Tamanho de amostra para estimar uma proporcao
- **Como chamar:**
```r
aas_tamanho_proporcao(p = 0.6, d = 0.03, conf = 0.95)
```
- **Pacotes necessários:** Nenhum

### `aas_tamanho_amostra()`

- **Inputs:** parametro: 'media'/'proporcao' | N, sigma2, sigma, p, d, conf, perda, arredondar (conforme o parametro)
- **Descrição:** Funcao guarda-chuva: decide sozinha entre aas_tamanho_media()/aas_tamanho_proporcao() e ignora argumentos que sobrarem
- **Como chamar:**
```r
aas_tamanho_amostra(parametro = 'media', sigma = 7, d = 2)
```
- **Pacotes necessários:** Nenhum

### `aas_ajustar_perdas()`

- **Inputs:** n: tam. base | taxa_acrescimo: proporcao (ex. 0.25) | arredondar
- **Descrição:** Acresce um percentual simples ao tamanho de amostra (Exemplo 3.14: n*(1+taxa))
- **Como chamar:**
```r
aas_ajustar_perdas(357, 0.25)
```
- **Pacotes necessários:** Nenhum

### `aas_comparar_reposicao()`

- **Inputs:** N: tam. populacao | n: tam. amostra
- **Descrição:** Calcula o EPA (efeito do planejamento amostral) comparando AASc x AASs
- **Como chamar:**
```r
aas_comparar_reposicao(N = 2695, n = 336)
```
- **Pacotes necessários:** Nenhum


## Cap. 4 - Estratificada

### `estr_estima_media()`

- **Inputs:** Nk, xk_bar, sk2, nk: vetores por estrato | conf
- **Descrição:** Estima a media populacional estratificada, variancia e IC
- **Como chamar:**
```r
estr_estima_media(Nk=c(1540,770,385), xk_bar=c(120,118,125), sk2=c(7,9,11), nk=c(69,34,17))
```
- **Pacotes necessários:** Nenhum

### `estr_estima_proporcao()`

- **Inputs:** Nk, pk, nk: vetores por estrato | conf
- **Descrição:** Estima a proporcao populacional estratificada, variancia e IC
- **Como chamar:**
```r
estr_estima_proporcao(Nk=c(1540,770,385), pk=c(.75,.50,.25), nk=c(161,81,40))
```
- **Pacotes necessários:** Nenhum

### `estr_alocacao_proporcional()`

- **Inputs:** n: tam. total | Nk: vetor por estrato
- **Descrição:** Distribui n entre estratos proporcionalmente ao tamanho de cada um
- **Como chamar:**
```r
estr_alocacao_proporcional(n = 120, Nk = c(1540,770,385))
```
- **Pacotes necessários:** Nenhum

### `estr_alocacao_neyman()`

- **Inputs:** n | Nk | sigmak (media) ou pk (proporcao)
- **Descrição:** Aloca a amostra entre estratos pela alocacao otima de Neyman
- **Como chamar:**
```r
estr_alocacao_neyman(n = 261, Nk = c(1500,2700,3300), pk = c(.10,.20,.40))
```
- **Pacotes necessários:** Nenhum

### `estr_alocacao_custo()`

- **Inputs:** n | Nk | sigmak ou pk | ck: custo por unidade em cada estrato
- **Descrição:** Aloca a amostra minimizando variancia para um orcamento fixo (custos desiguais por estrato)
- **Como chamar:**
```r
estr_alocacao_custo(n=491, Nk=c(15000,10000,5000), sigmak=c(3,3,2), ck=c(10,10,20))
```
- **Pacotes necessários:** Nenhum

### `estr_tamanho_media()`

- **Inputs:** Nk, sigmak2 | d | conf | ck (opcional) | alocacao: proporcional/neyman/custo/nenhuma | arredondar
- **Descrição:** Tamanho de amostra total (e por estrato) para estimar uma media estratificada
- **Como chamar:**
```r
estr_tamanho_media(Nk=c(1540,770,385), sigmak2=c(7,9,11), d=0.5, alocacao='neyman')
```
- **Pacotes necessários:** Nenhum

### `estr_tamanho_proporcao()`

- **Inputs:** Nk, pk | d | conf | ck (opcional) | alocacao | arredondar
- **Descrição:** Tamanho de amostra total (e por estrato) para estimar uma proporcao estratificada
- **Como chamar:**
```r
estr_tamanho_proporcao(Nk=c(500,920,430,200), pk=c(.10,.50,.30,.10), d=0.05)
```
- **Pacotes necessários:** Nenhum

### `estr_tamanho_amostra()`

- **Inputs:** parametro: 'media'/'proporcao' | Nk, sigmak2, pk, d, conf, ck, alocacao, arredondar
- **Descrição:** Funcao guarda-chuva: decide entre estr_tamanho_media()/estr_tamanho_proporcao()
- **Como chamar:**
```r
estr_tamanho_amostra(parametro='media', Nk=c(1540,770,385), sigmak2=c(7,9,11), d=0.5)
```
- **Pacotes necessários:** Nenhum

### `amostragem_ajustar_resposta()`

- **Inputs:** n: tam. base | taxa_resposta: taxa minima esperada (ex. 0.85)
- **Descrição:** Ajusta o tamanho de amostra para garantir n respostas completas (divide por taxa_resposta)
- **Como chamar:**
```r
amostragem_ajustar_resposta(1525, 0.85)
```
- **Pacotes necessários:** Nenhum


## Cap. 5 - Sistematica

### `sist_intervalo()`

- **Inputs:** N: tam. populacao | n: tam. amostra desejado
- **Descrição:** Calcula o intervalo de selecao k (e a correcao k* se N/n nao for inteiro)
- **Como chamar:**
```r
sist_intervalo(N = 2000, n = 75)
```
- **Pacotes necessários:** Nenhum

### `sist_selecionar_amostra()`

- **Inputs:** N | n | r: inicio aleatorio (opcional, sorteado se omitido) | semente (opcional)
- **Descrição:** Seleciona a amostra sistematica completa (posicoes 1..N)
- **Como chamar:**
```r
sist_selecionar_amostra(N = 2000, n = 75, r = 26)
```
- **Pacotes necessários:** Nenhum


## Cap. 6 - Conglomerados (1 etapa)

### `cong1_estima_media()`

- **Inputs:** yi: totais por conglomerado | Mi: tam. de cada conglomerado | Nc: nº total de conglomerados | M (opcional) | conf
- **Descrição:** Estima a media populacional por conglomerados em uma etapa
- **Como chamar:**
```r
cong1_estima_media(yi=..., Mi=..., Nc=373, M=8018)
```
- **Pacotes necessários:** Nenhum

### `cong1_estima_proporcao()`

- **Inputs:** yi: 'sucessos' por conglomerado | Mi | Nc | M (opcional) | conf
- **Descrição:** Estima uma proporcao populacional por conglomerados em uma etapa
- **Como chamar:**
```r
cong1_estima_proporcao(yi=..., Mi=..., Nc=373, M=8018)
```
- **Pacotes necessários:** Nenhum

### `cong1_tamanho_media()`

- **Inputs:** Nc | sigmac2: variancia entre conglomerados | M_barra | d | conf | arredondar
- **Descrição:** Nº de conglomerados necessario para estimar uma media
- **Como chamar:**
```r
cong1_tamanho_media(Nc=373, sigmac2=51054.39, M_barra=8018/373, d=3)
```
- **Pacotes necessários:** Nenhum

### `cong1_tamanho_proporcao()`

- **Inputs:** Nc | sigmacp2 | M_barra | d | conf | arredondar
- **Descrição:** Nº de conglomerados necessario para estimar uma proporcao
- **Como chamar:**
```r
cong1_tamanho_proporcao(Nc=373, sigmacp2=0.9179, M_barra=8018/373, d=0.013)
```
- **Pacotes necessários:** Nenhum

### `cong1_ajustar_deff()`

- **Inputs:** n_prime: tam. base | deff (default 1.5) | taxa_resposta (default 0.85)
- **Descrição:** Alias de amostragem_ajustar_deff_resposta() (ver funcao transversal abaixo)
- **Como chamar:**
```r
cong1_ajustar_deff(n_prime = 518, deff = 1.5, taxa_resposta = 0.85)
```
- **Pacotes necessários:** Nenhum


## Cap. 6 - Conglomerados (2 etapas)

### `cong2_estima_media()`

- **Inputs:** dados (data.frame) OU Mi/mi/yi_bar/si2 (resumidos) | Nc | M (conhecido) | conf
- **Descrição:** Estima a media populacional por conglomerados em 2 etapas, com M conhecido
- **Como chamar:**
```r
cong2_estima_media(Mi=Mi, mi=mi, yi_bar=yi_bar, si2=si2, Nc=90, M=4500)
```
- **Pacotes necessários:** Nenhum

### `cong2_estima_razao_media()`

- **Inputs:** dados OU Mi/mi/yi_bar/si2 | Nc | conf
- **Descrição:** Estimador razao da media em 2 etapas, quando M e desconhecido
- **Como chamar:**
```r
cong2_estima_razao_media(Mi=Mi, mi=mi, yi_bar=yi_bar, si2=si2, Nc=90)
```
- **Pacotes necessários:** Nenhum

### `cong2_estima_proporcao()`

- **Inputs:** Mi | mi | pi_hat: proporcao por conglomerado | Nc | conf
- **Descrição:** Estima uma proporcao populacional em 2 etapas (estrutura tipo razao, usa m_barra amostral)
- **Como chamar:**
```r
cong2_estima_proporcao(Mi=Mi, mi=mi, pi_hat=pi_hat, Nc=90)
```
- **Pacotes necessários:** Nenhum

### `cong_estima()`

- **Inputs:** mesmos de cong2_estima_media()/razao_media(), M opcional
- **Descrição:** Guarda-chuva: usa M conhecido se informado, senao usa o estimador razao automaticamente
- **Como chamar:**
```r
cong_estima(Mi=Mi, mi=mi, yi_bar=yi_bar, si2=si2, Nc=90)  # M ausente -> razao
```
- **Pacotes necessários:** Nenhum


## Transversal

### `amostragem_ajustar_deff_resposta()`

- **Inputs:** n_prime: tam. base | deff (default 1.5) | taxa_resposta (default 0.85)
- **Descrição:** Ajusta qualquer tamanho de amostra base por efeito de delineamento (deff) + taxa de resposta -- nao exclusivo de conglomerados
- **Como chamar:**
```r
aas_tamanho_proporcao(p=.2, d=.05)$estimativa$n |> amostragem_ajustar_deff_resposta(deff=1.2, taxa_resposta=.85)
```
- **Pacotes necessários:** Nenhum

### `amostragem_comparar_planos()`

- **Inputs:** ...: 2+ objetos sampling_result nomeados | referencia (nome do plano-base, opcional)
- **Descrição:** Compara a variancia de diferentes planos amostrais (calcula EPA/design effect entre eles)
- **Como chamar:**
```r
amostragem_comparar_planos(aas = res_aas, estrat = res_estrat)
```
- **Pacotes necessários:** Nenhum


## Cap. 7 - Razao e Regressao

### `reg_ajustar()`

- **Inputs:** x: variavel auxiliar | y: variavel de interesse
- **Descrição:** Ajusta a regressao linear simples y~x (minimos quadrados) e testa a origem
- **Como chamar:**
```r
reg_ajustar(x, y)
```
- **Pacotes necessários:** Nenhum

### `reg_teste_origem()`

- **Inputs:** x | y | conf
- **Descrição:** Testa se a reta passa pela origem (decide entre estimador razao ou regressao)
- **Como chamar:**
```r
reg_teste_origem(x, y)
```
- **Pacotes necessários:** Nenhum

### `reg_estima_media()`

- **Inputs:** x | y | X_barra: media populacional da variavel auxiliar | N (opcional) | conf
- **Descrição:** Estimador regressao da media populacional
- **Como chamar:**
```r
reg_estima_media(x, y, X_barra = 52, N = 486)
```
- **Pacotes necessários:** Nenhum

### `razao_estima_R()`

- **Inputs:** x,y (dados brutos) OU somatorios (n,sum_x,sum_y,sum_x2,sum_y2,sum_xy) | N | X | conf
- **Descrição:** Estima a razao populacional R=Y/X e sua variancia
- **Como chamar:**
```r
razao_estima_R(x = x, y = y, N = 1500, X = 15000)
```
- **Pacotes necessários:** Nenhum

### `razao_estima_total()`

- **Inputs:** x,y ou somatorios | N | X | conf
- **Descrição:** Estimador razao do total populacional
- **Como chamar:**
```r
razao_estima_total(x = x, y = y, N = 1500, X = 15000)
```
- **Pacotes necessários:** Nenhum

### `razao_estima_media()`

- **Inputs:** x,y ou somatorios | N | X (opcional) | conf
- **Descrição:** Estimador razao da media populacional
- **Como chamar:**
```r
razao_estima_media(x = x, y = y, N = 1500)
```
- **Pacotes necessários:** Nenhum

### `razao_variancia_piloto()`

- **Inputs:** x,y ou somatorios (amostra piloto)
- **Descrição:** Calcula sr^2 (variancia piloto do estimador razao), insumo para as funcoes de tamanho de amostra abaixo
- **Como chamar:**
```r
razao_variancia_piloto(x = x, y = y)
```
- **Pacotes necessários:** Nenhum

### `razao_tamanho_R()`

- **Inputs:** sr2 | N | X_barra | d | conf | arredondar
- **Descrição:** Tamanho de amostra para estimar a razao R
- **Como chamar:**
```r
razao_tamanho_R(sr2 = 7.7853, N = 1500, X_barra = 10, d = 0.05)
```
- **Pacotes necessários:** Nenhum

### `razao_tamanho_total()`

- **Inputs:** sr2 | N | d | conf | arredondar
- **Descrição:** Tamanho de amostra para estimar o total via estimador razao
- **Como chamar:**
```r
razao_tamanho_total(sr2 = 7.7853, N = 1500, d = 500)
```
- **Pacotes necessários:** Nenhum

### `razao_tamanho_media()`

- **Inputs:** sr2 | N | d | conf | arredondar
- **Descrição:** Tamanho de amostra para estimar a media via estimador razao
- **Como chamar:**
```r
razao_tamanho_media(sr2 = 2.2462, N = 1000, d = 0.75)
```
- **Pacotes necessários:** Nenhum

### `razao_vs_regressao_decidir()`

- **Inputs:** x | y | X_barra | N | conf
- **Descrição:** Guarda-chuva: roda o teste de origem e calcula razao E regressao lado a lado, indicando a recomendada
- **Como chamar:**
```r
razao_vs_regressao_decidir(x, y, X_barra = 10, N = 1500)
```
- **Pacotes necessários:** Nenhum


## Utilitario (S3)

### `print(objeto)`

- **Inputs:** objeto: qualquer resultado de funcao do sampleone (classe sampling_result)
- **Descrição:** Mostra estimativa, variancia, IC e interpretacao de forma resumida
- **Como chamar:**
```r
resultado <- aas_tamanho_media(sigma=7, d=2)
print(resultado)  # ou so: resultado
```
- **Pacotes necessários:** Nenhum

### `summary(objeto)`

- **Inputs:** objeto: qualquer resultado de funcao do sampleone
- **Descrição:** Mostra tudo do print() + memoria de calculo passo a passo + referencias bibliograficas
- **Como chamar:**
```r
resultado |> summary()
```
- **Pacotes necessários:** Nenhum
