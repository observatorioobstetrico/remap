# Atualizacao dos obitos de gestantes e puerperas

Este fluxo atualiza apenas as bases usadas pelas telas:

- `Obitos > Oficiais`
- `Obitos > Nao considerados`

Ele foi criado para incluir `municipio_ocorrencia`, obtido diretamente do
campo `CODMUNOCOR` do SIM. O script nunca deriva municipio de ocorrencia a
partir do municipio de residencia, do tipo de local de ocorrencia ou de outra
variavel.

## Comando principal

Rode a partir da raiz do projeto:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/obitos_grav_puerp/update_obitos_grav_puerp.R'
```

Por padrao, o comando:

1. baixa ou reutiliza os brutos do SIM;
2. processa residentes no estado de Sao Paulo;
3. gera os CSVs candidatos em `inst/scripts/obitos_grav_puerp/outputs/`;
4. compara a extracao nova com os CSVs do painel de referencia
   `painel-obitos-grav-puerp-main`, filtrados aos municipios de SP que o app
   consome;
5. interrompe a execucao se houver divergencia contra a referencia;
6. se a validacao passar, substitui os CSVs usados pelo app em
   `inst/app/data/`.

## Rebuild sem novo download

Use quando os arquivos brutos ja existirem em `data-raw/obitos_grav_puerp/raw/`:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/obitos_grav_puerp/update_obitos_grav_puerp.R' --rebuild-only
```

## Gerar candidatos sem substituir o app

Use para testar a extracao e olhar os relatórios antes de atualizar o painel:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/obitos_grav_puerp/update_obitos_grav_puerp.R' --no-apply
```

## Anos processados

Padrao:

- historico consolidado: `1996:2022`, via `microdatasus::fetch_datasus()`
- preliminares: `2023:2025`, via CSVs abertos do SIM no S3 do Ministerio da Saude

Para restringir anos em testes:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/obitos_grav_puerp/update_obitos_grav_puerp.R' --historical-years=2022:2022 --preliminary-years=2025:2025 --no-apply
```

## Fontes

Historico consolidado:

- sistema: SIM-DO
- pacote: `microdatasus`
- UF de residencia: `SP`
- variaveis essenciais: `CODMUNRES`, `CODMUNOCOR`, `DTOBITO`, `IDADE`, `SEXO`,
  `CAUSABAS`, `OBITOGRAV`, `OBITOPUERP`, `RACACOR`, `ESTCIV`, `LOCOCOR`,
  `ASSISTMED`, `NECROPSIA`, `ESC2010`, `FONTEINV`

Preliminares:

- `https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO23OPEN.csv`
- `https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv`
- `https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/csv/DO25OPEN_csv.zip`

## Arquivos auxiliares

O pipeline usa estes arquivos locais:

- `inst/scripts/obitos_grav_puerp/auxiliary/df_cid10.csv`
- `inst/scripts/obitos_grav_puerp/auxiliary/df_aux_municipios.csv`

`df_aux_municipios.csv` e importante porque contem codigos historicos e
codigos ignorados do SIM. Isso evita mapear `CODMUNOCOR` por aproximacao.
Se uma atualizacao futura encontrar codigo sem correspondencia, o script para
e lista os codigos que precisam ser incorporados ao lookup.

## Validacoes geradas

Os arquivos brutos baixados ficam em `data-raw/obitos_grav_puerp/raw/`.
Essa pasta fica fora do build do pacote por causa do `.Rbuildignore`, o que
evita levar os CSVs preliminares grandes para o deploy.

Os relatorios ficam em:

`inst/scripts/obitos_grav_puerp/outputs/validacao/`

Arquivos principais:

- `resumo_validacao.csv`: resultado geral por base e comparacao
- `validacao_por_ano.csv`: diferencas ano a ano contra a referencia
- `diferencas_*.csv`: criado apenas quando ha divergencias de combinacoes
- `municipio_ocorrencia_nao_informado.csv`: criado se houver obitos sem
  `CODMUNOCOR` mapeavel

A validacao de sucesso e:

- `autochecagem_sem_municipio_ocorrencia` batendo exatamente;
- `painel_referencia` batendo exatamente para as duas bases.

## Quando a referencia divergir

Nao use `--allow-reference-mismatch` para atualizar o app sem antes investigar.
Essa opcao existe apenas para auditoria, por exemplo quando a fonte oficial
mudar depois do painel de referencia.

Se houver divergencia:

1. confira `validacao_por_ano.csv`;
2. abra o `diferencas_*.csv` correspondente;
3. confirme se o painel de referencia tambem foi atualizado;
4. so depois rode novamente.

## Saidas do app

Quando a validacao passa, estes arquivos sao substituidos:

- `inst/app/data/dados_oobr_obitos_grav_puerp_maternos_oficiais_1996_2025.csv`
- `inst/app/data/dados_oobr_obitos_grav_puerp_desconsiderados_1996_2025.csv`

O cache de `load_obitos_data()` observa a data desses CSVs. Ainda assim, ao
testar no Shiny, feche e reabra o app para evitar uma sessao antiga com cache
em memoria.
