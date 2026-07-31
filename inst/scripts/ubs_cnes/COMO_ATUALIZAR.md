# Atualizacao do numero de UBS na APS

Este fluxo atualiza o indicador **Numero de Unidades Basicas de Saude (UBS)** da tela de Atencao Primaria a Saude.

## Fonte

- Portal: `https://datasus.saude.gov.br/transferencia-de-arquivos/#`
- Grupo: CNES
- Base: `ST - Estabelecimentos`
- Diretorio FTP usado pelo script: `ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST`
- Arquivos: `STSPYY12.dbc`, sempre a competencia de dezembro de cada ano.

## Regra de contagem

Por enquanto, o indicador considera apenas estabelecimentos com:

- `TP_UNID == "02"`
- descricao: `UNIDADE BASICA DE SAUDE`

A contagem final e o numero distinto de `CNES` por municipio e ano. Outros tipos de estabelecimento nao entram nesta versao.

## Periodo atual

Como 2026 ainda nao possui dezembro disponivel, a grade atual do painel e:

- 2022
- 2023
- 2024
- 2025

Todos os anos sao tratados como consolidados porque a competencia de dezembro ja existe na fonte ST.

## Escopo territorial no painel

A extracao e municipal. As visualizacoes por Estado, RRAS, DRS e Regiao de Saude sao agregacoes dos municipios usando `inst/app/data/RRAS-MUNICIPIO.xlsx`.

As visualizacoes baseadas em Supervisao de Saude nao foram atualizadas nesta etapa, pois a base ST utilizada aqui nao traz esse recorte diretamente no contrato do painel.

## Como rodar

Para baixar os arquivos brutos e reconstruir tudo:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla inst/scripts/ubs_cnes/update_ubs_cnes.R
```

Para reconstruir usando os `.dbc` ja baixados em `inst/app/data/ubs_cnes/raw`:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla inst/scripts/ubs_cnes/update_ubs_cnes.R --rebuild-only
```

## Saidas

- `inst/app/data/ubs_cnes_aps.rda`: objeto `aps_ubs_cnes`, consumido pelo painel.
- `inst/app/data/ubs_cnes_aps.xlsx`: planilha de auditoria com abas `municipal`, `validacao` e `metodo`.
- `inst/scripts/ubs_cnes/metadata_ultima_atualizacao.json`: resumo da ultima geracao.

## Validacoes feitas pelo script

- Verifica se todos os arquivos `STSPYY12.dbc` esperados existem.
- Verifica a presenca das colunas `CNES`, `CODUFMUN` e `TP_UNID`.
- Interrompe a execucao se algum municipio da base ST/SP nao tiver correspondencia em `RRAS-MUNICIPIO.xlsx`.
- Gera totais por ano para conferir quantidade de municipios com UBS e total de UBS contabilizadas.
