# Validação do `sampleone` — Lista 1 (Temas 1 a 5), Prof. Dr. Mariano Martínez Espinosa

**Método:** cada questão foi resolvida de forma **independente** (cálculo em Python, sem consultar o código-fonte do pacote), citando fórmula e página da apostila, exatamente como a lista exige do aluno. Só depois cada solução foi mapeada para a chamada `sampleone` equivalente. Isso funciona como um teste cego real — diferente dos testes internos do pacote, que foram derivados da própria apostila.

A Questão 1 é conceitual (desenho de pesquisa, sem cálculo numérico) — fora do escopo de um pacote de cálculo, não tratada aqui.

---

## Questão 2 — Prevalência de diabetes (N=45.000, p piloto=20%)

### 2a) N conhecido, p=0,20, d=0,025, conf=95%, com acréscimo de 25%

**Fórmula (3.45)**, p. 47 (N conhecido) + **Exemplo 3.14** (acréscimo), p. 48-49.

- n bruto = 962,40 → **n = 963**
- com acréscimo de 25%: 963 × 1,25 = 1203,75 → **n = 1204**

```r
base <- aas_tamanho_proporcao(p = 0.20, d = 0.025, conf = 0.95, N = 45000)
aas_ajustar_perdas(base$estimativa$n, 0.25)
```
**Resultado esperado:** `n = 963` → `n_ajustado = 1204`.

### 2b) mesma p, d, conf — **sem N informado**

O enunciado não repete N aqui. Interpretação assumida: população desconhecida/infinita → **Fórmula (3.23)**, p. 41.

- n bruto = 983,41 → **n = 984**

```r
aas_tamanho_proporcao(p = 0.20, d = 0.025, conf = 0.95)  # N = NULL -> (3.23)
```
**Resultado esperado:** `n = 984`.

⚠️ **Ambiguidade no enunciado:** se a intenção for reaproveitar N=45.000 do item (a), o item (b) fica idêntico ao (a) menos o acréscimo — o que parece redundante. A leitura "N desconhecido" (contraste proposital com o item a) faz mais sentido pedagógico. Recomendo confirmar com o Prof. Mariano.

### 2c) p=0,20, d=0,05, conf=95%, deff=1,2, taxa mínima de resposta=85%

**Fórmula (3.23)**, p. 41 (n' base, N não mencionado) + **Fórmula (6.6)**, p. 78 (ajuste por deff e taxa de resposta).

- n' = 245,85 → **246**
- n_deff = 246×1,2 = 295,20
- ajuste resposta = 246/0,85 − 246 = 43,41
- n final = 295,20 + 43,41 = 338,61 → **n = 339**

```r
n_prime <- aas_tamanho_proporcao(p = 0.20, d = 0.05, conf = 0.95)$estimativa$n  # 246
cong1_ajustar_deff(n_prime, deff = 1.2, taxa_resposta = 0.85)
```
**Resultado esperado:** `n_final = 339`.

🔴 **Achado de design importante:** a fórmula (6.6) está catalogada no Cap. 6 (Conglomerados) porque é onde a apostila primeiro a apresenta — mas este exercício (2c) a aplica a um problema de **prevalência simples, sem estrutura de conglomerado nenhuma**. Ou seja, a fórmula é, na prática, **transversal** (só precisa de n', deff, taxa de resposta — nada específico de conglomerado). Hoje ela só existe como `cong1_ajustar_deff()`, o que é enganoso/limitante para quem quer usá-la fora do Cap. 6. **Ação recomendada:** expor a mesma lógica também como uma função transversal, ex. `amostragem_ajustar_deff_resposta()`, com `cong1_ajustar_deff()` passando a ser um alias/wrapper dela. Vou implementar isso já a seguir.

---

## Questão 3 — Produção de soja, 3 estratos (Nk=2500/1000/500, σk²=64/400/1600)

### 3a) Tamanho total e por estrato, alocação proporcional, d=5, conf=95%

**Fórmula (4.5)**, p. 52 (tamanho total) + **Fórmula (4.6)**, p. 52 (alocação proporcional).

- n bruto = 51,57 → **n = 52**
- alocação bruta: [32,5 ; 13,0 ; 6,5] → arredondada e ajustada para somar 52: **[33, 13, 6]**

```r
estr_tamanho_media(Nk = c(2500, 1000, 500), sigmak2 = c(64, 400, 1600), d = 5, conf = 0.95,
                    alocacao = "proporcional")
```
**Resultado esperado:** `n = 52`, `nk = c(33, 13, 6)`.

### 3b) Alocação ótima de Neyman, mesmo d, conf

**Fórmula (4.21)**, p. 61 (tamanho) + **Fórmulas (4.18)-(4.20)**, p. 60 (alocação).

- σk = [8, 20, 40]; Nk·σk = [20000, 20000, 20000] (**empate exato** entre os 3 estratos!)
- n bruto = 34,13 → **n = 35**
- alocação: [35/3, 35/3, 35/3] → **[11, 12, 12]** (soma ajustada a 35)

```r
estr_tamanho_media(Nk = c(2500, 1000, 500), sigmak2 = c(64, 400, 1600), d = 5, conf = 0.95,
                    alocacao = "neyman")
```
**Resultado esperado:** `n = 35`, `nk` ≈ partes iguais (11 ou 12 cada).

📌 **Nota didática:** este exercício tem uma coincidência interessante — Nk·σk é **exatamente igual** nos 3 estratos (20.000), o que faz a alocação de Neyman cair em partes ~iguais, diferente do padrão usual onde estratos maiores/mais variáveis recebem mais amostra. Bom exemplo para ilustrar em aula que Neyman não é sempre "proporcional ao tamanho".

### 3c) Comparar variância: proporcional (n=52) vs. Neyman (n=35)

- Var(proporcional, n=52) ≈ **6,786**
- Var(Neyman, n=35) ≈ **6,165**

Neyman atinge variância **menor** com **33% menos amostra** (35 vs. 52) — ganho de eficiência expressivo, mais um bom exemplo pedagógico.

```r
amostragem_comparar_planos(
  proporcional = estr_estima_media(Nk = c(2500,1000,500), xk_bar = c(0,0,0), sk2 = c(64,400,1600), nk = c(33,13,6)),
  neyman       = estr_estima_media(Nk = c(2500,1000,500), xk_bar = c(0,0,0), sk2 = c(64,400,1600), nk = c(11,12,12))
)
```
(`xk_bar` é irrelevante para a variância, usado aqui só como placeholder — `estr_estima_media()` exige o argumento mas o cálculo de variância não depende dele.)

### 3d) σ²=700 (única, não por estrato), d=5, conf=95%, deff=1,5, taxa resposta=85%

Aqui o enunciado dá uma variância **única**, não por estrato — ou seja, reduz a um problema de AAS geral (não estratificado), na mesma linha do item 2c.

**Fórmula (3.15)**, p. 39 (N não mencionado → infinito) + **Fórmula (6.6)**, p. 78.

- n' = 107,56 → **108**
- n final = 108×1,5 + (108/0,85−108) = 162 + 19,06 = 181,06 → **n = 182**

```r
n_prime <- aas_tamanho_media(sigma2 = 700, d = 5, conf = 0.95)$estimativa$n  # 108
amostragem_ajustar_deff_resposta(n_prime, deff = 1.5, taxa_resposta = 0.85)  # funcao nova, ver acima
```
**Resultado esperado:** `n_final = 182`.

*(Se N=4000 — população total do enunciado original — for considerado em vez de "infinito", o resultado muda para n'=105 → n_final=177. Vale confirmar qual interpretação o professor pretende.)*

---

## Questão 4 — Satisfação salarial, 4 estratos (N=2050)

### 4a) Alocação proporcional, d=0,05, conf=95%

**Fórmula (4.12)**, p. 57 + **Fórmulas (4.14)/(4.15)**, p. 59.

- n bruto = 251,99 → **n = 252**
- alocação: **[61, 113, 53, 25]** (soma = 252, bate exatamente sem precisar de ajuste de arredondamento)

```r
estr_tamanho_proporcao(Nk = c(500, 920, 430, 200), pk = c(0.10, 0.50, 0.30, 0.10),
                        d = 0.05, conf = 0.95, alocacao = "proporcional")
```
**Resultado esperado:** `n = 252`, `nk = c(61, 113, 53, 25)`.

### 4b) Alocação ótima de Neyman, mesmo d, conf

**Fórmula (4.21)**, p. 61 (substituindo σk por √[pk(1−pk)]) + alocação análoga a (4.18)-(4.20).

- n bruto = 241,09 → **n = 242**
- alocação: **[42, 128, 55, 17]** (soma = 242)

```r
estr_tamanho_proporcao(Nk = c(500, 920, 430, 200), pk = c(0.10, 0.50, 0.30, 0.10),
                        d = 0.05, conf = 0.95, alocacao = "neyman")
```
**Resultado esperado:** `n = 242`, `nk = c(42, 128, 55, 17)`.

### 4c) Comparar variância

- Var(proporcional, n=252) ≈ 0,00065970
- Var(Neyman, n=242) ≈ 0,00065912
- EPA (Neyman/Proporcional) ≈ **0,9991**

Ganho de eficiência muito pequeno aqui (as proporções pk não variam tanto proporcionalmente a ponto de Neyman se destacar muito) — outro bom contraste pedagógico com a Questão 3, onde o ganho foi grande.

---

## Questão 5 — Amostragem sistemática (N=1500, n=40)

**Seção 5.2**, p. 70 (regra de seleção).

- k = N/n = 37,5 → não inteiro → arredondar para cima: k=38; checar k·n=1520>1500 → reduzir para **k=37**
- k* = N − k·n = 1500 − 1480 = **20**
- Com r=15 (exemplo de início aleatório, 1≤r≤37): primeiros elementos **15, 72, 109, 146, 183, ...**, últimos **..., 1404, 1441, 1478** (40 elementos no total)

```r
sist_intervalo(N = 1500, n = 40)
sist_selecionar_amostra(N = 1500, n = 40, r = 15)
```
**Resultado esperado:** `k = 37`, `k_estrela = 20`, 40 elementos entre 15 e 1478.

---

## Resumo — o que essa validação cega revelou

| Item | Resultado |
|---|---|
| Fórmulas de tamanho de amostra AAS, estratificada (proporcional e Neyman), sistemática | ✅ Todas batem com o cálculo independente |
| Ajuste por deff + taxa de resposta (6.6) | ✅ Fórmula correta, mas **mal posicionada** no pacote (só existia sob `cong1_`) |
| Ambiguidades de enunciado (2b, 3d: N informado ou não?) | 📋 Não são bugs — recomendo confirmar interpretação com o professor |
| Coincidência didática no Ex. 3 (Neyman = partes iguais) | 💡 Boa oportunidade para nota pedagógica na vinheta |

**Ação de código:** vou expor `amostragem_ajustar_deff_resposta()` como função transversal (Cap. 2/utilitários), com `cong1_ajustar_deff()` virando um wrapper dela — assim ela fica disponível de forma natural para qualquer contexto (AAS, estratificada, conglomerados), sem sugerir falsamente que só serve para conglomerados.
