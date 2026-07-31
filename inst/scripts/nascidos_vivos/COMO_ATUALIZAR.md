# Atualizacao dos Nascidos vivos APS

O fluxo oficial de atualizacao dos dados de Nascidos vivos da tela de
Atencao Primaria a Saude fica em `inst/scripts/nascidos_vivos`.

## Comando principal

Rode a partir da raiz do projeto:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/nascidos_vivos/update_nascidos_vivos.R'
```

## Rebuild sem novo download

Se os arquivos brutos ja estiverem baixados e voce quiser apenas reconstruir
o Excel consolidado e o `.rda`:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/nascidos_vivos/update_nascidos_vivos.R' --rebuild-only
```

## Fontes usadas

1. Municipios do estado de Sao Paulo, anos 2020 a 2024:

`http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinasc/cnv/nvsp.def`

Consulta:

- Linha: `Municipio`
- Coluna: `Nao ativa`
- Conteudo: `Nascim p/resid.mae`
- Arquivos: `nvsp20.dbf` a `nvsp24.dbf`
- Formato: `prn`
- Opcao marcada: `Exibir linhas zeradas`

2. Municipio de Sao Paulo por supervisao de saude, anos 2020 a 2025:

`https://tabnet.saude.prefeitura.sp.gov.br/cgi/deftohtm3.exe?secretarias/saude/TABNET/sinasc/nascido.def`

Consulta:

- Linha: `Supervisao T. Saude residencia`
- Coluna: `Nao ativa`
- Conteudo: `NV parturientes residentes MSP`
- Arquivos: `dnsp20.dbf` a `dnsp25.dbf`
- Formato: `prn`
- Opcao marcada: `Exibir linhas zeradas`

## O que o script faz

1. baixa os PRNs dos dois TabNets para:

`inst/app/data/nascidos_vivos/raw/`

2. le os arquivos baixados e padroniza nomes
3. junta os municipios do DATASUS com `RRAS-MUNICIPIO.xlsx`
4. junta as supervisoes da Prefeitura de SP com as supervisoes existentes em `remap6.xlsx`
5. substitui o municipio de Sao Paulo do DATASUS pelo total oficial da Prefeitura de SP, mantendo o valor `Ignorado` quando ele vem informado pelo TabNet
6. confere se o total oficial da Prefeitura de SP bate com a soma das supervisoes mais `Ignorado`
7. calcula agregados por coordenadoria de saude a partir das supervisoes identificadas, sem redistribuir `Ignorado`
8. grava a base documentavel em:

`inst/app/data/nascidos_vivos_aps.xlsx`

9. grava a base consumida pelo app em:

`inst/app/data/nascidos_vivos_aps.rda`

## Regras de uso no painel

- Graficos por municipio usam 2020 a 2024, todos consolidados.
- Graficos por supervisao de saude usam 2020 a 2024 como consolidados e 2025 como preliminar.
- O municipio de Sao Paulo sempre usa o total oficial da Prefeitura de SP, que inclui `Ignorado` quando essa linha existe na consulta.
- A base mantem `Ignorado` para fechar os totais oficiais, mas os graficos ocultam essa categoria. O valor nao e redistribuido entre supervisoes ou coordenadorias.
- RRAS 6 e regioes/coordenadorias/supervisoes dentro do municipio de Sao Paulo podem exibir 2025 preliminar.
- Os demais contextos agregados usam 2024 como ultimo ano consolidado.

## Inclusao de novos anos

Para incluir novos anos no futuro, ajuste os vetores `municipal_years` e
`sp_years` na chamada de `build_nascidos_vivos_aps_data()` em
`nascidos_vivos_pipeline.R`, rode o comando principal e confira o Excel gerado.

## Observacoes

- Feche qualquer instancia do app antes de atualizar os dados para evitar cache
  travado em `inst/app/data/_rda/`.
- O arquivo `metadata_ultima_atualizacao.json` registra a ultima execucao.
- A base nao altera as planilhas `remap*.xlsx`; o app passa a consumir o novo
  `.rda` apenas para o indicador de Nascidos vivos.
