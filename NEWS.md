# sampleone 0.1.0

* Initial release, implementing all technical chapters (2-7) of the
  source course notes ("Nocoes de Amostragem", DEs-ICET-UFMT).
* Cap. 2 (conceitos base): `pop_total()`, `pop_media()`, `pop_proporcao()`,
  `amostra_estimadores()`.
* Cap. 3 (amostragem aleatoria simples): `aas_estima_media()`,
  `aas_estima_total()`, `aas_estima_proporcao()`, `aas_tamanho_media()`,
  `aas_tamanho_proporcao()`, `aas_tamanho_amostra()`, `aas_ajustar_perdas()`,
  `aas_comparar_reposicao()`.
* Cap. 4 (amostragem estratificada): `estr_estima_media()`,
  `estr_estima_proporcao()`, `estr_alocacao_proporcional()`,
  `estr_alocacao_neyman()`, `estr_alocacao_custo()`, `estr_tamanho_media()`,
  `estr_tamanho_proporcao()`, `estr_tamanho_amostra()`,
  `amostragem_ajustar_resposta()`.
* Cap. 5 (amostragem sistematica): `sist_intervalo()`,
  `sist_selecionar_amostra()`.
* Cap. 6 (conglomerados): `cong1_estima_media()`, `cong1_estima_proporcao()`,
  `cong1_tamanho_media()`, `cong1_tamanho_proporcao()`,
  `cong1_ajustar_deff()`, `cong2_estima_media()`,
  `cong2_estima_razao_media()`, `cong2_estima_proporcao()`, `cong_estima()`.
* Cap. 7 (razao e regressao): `reg_ajustar()`, `reg_teste_origem()`,
  `reg_estima_media()`, `razao_estima_R()`, `razao_estima_total()`,
  `razao_estima_media()`, `razao_variancia_piloto()`, `razao_tamanho_R()`,
  `razao_tamanho_total()`, `razao_tamanho_media()`,
  `razao_vs_regressao_decidir()`.
* Cross-cutting utility: `amostragem_comparar_planos()`.
* Shared S3 class `sampling_result` with `print()` and `summary()` methods
  used by every function above.
* Vignette `catalogo-formulas` mapping every course-notes equation to its
  corresponding package function.
* 111 testthat tests, each reproducing a specific worked numerical example
  from the source course notes.
