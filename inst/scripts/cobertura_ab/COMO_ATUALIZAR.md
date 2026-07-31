# Atualizacao das Coberturas APS

O fluxo oficial de atualizacao das coberturas APS agora fica todo dentro de `inst`.

## Comando principal

Rode a partir da raiz do projeto:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/cobertura_ab/update_cobertura_ab.R'
```

## O que o script faz

1. consulta as competencias disponiveis em `https://relatorioaps-prd.saude.gov.br/data/competencias-cnes/2`
2. baixa a base da APS para `MUNICIPIO` e `coUf = 35` diretamente de `https://relatorioaps-prd.saude.gov.br/cobertura/aps`
3. salva o bruto em:

`inst/app/data/cobertura_ab/raw/cobertura_ab_aps_municipio_sp_raw.rds`

4. consolida a base anual da Cobertura AB e da Cobertura ESF para todos os anos completos disponiveis
5. grava a base consolidada documentavel em:

`inst/app/data/cobertura_ab_aps.xlsx`

6. gera o arquivo consumido pelo app a partir do Excel consolidado:

`inst/app/data/cobertura_ab_aps.rda`

## Rebuild sem novo download

Se voce ja tiver baixado o bruto e quiser apenas reconstruir o `.xlsx` consolidado e o `.rda`:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/cobertura_ab/update_cobertura_ab.R' --rebuild-only
```

## Regras da consolidacao

O script replica a logica usada como referencia para o painel:

1. usa os registros mensais da APS no nivel municipal
2. usa o ano da `competencia CNES` como ano efetivo da base nova
3. considera apenas anos com 12 competencias completas
4. calcula a media anual de `qtCapacidadeEquipe`, `qtEsf` e `qtPopulacao` por `ano + municipio`
5. calcula a Cobertura AB a partir de `qtCapacidadeEquipe`
6. calcula a Cobertura ESF pela formula oficial `qtEsf * 3500 / populacao`
7. limita os numeradores ao maximo de `qt_populacao`
8. junta RRAS, DRS e Regiao de Saude a partir de `RRAS-MUNICIPIO.xlsx`

## Como os anos aparecem no painel

- O historico de `2020` continua vindo das planilhas legadas do painel.
- Os anos completos vindos do e-Gestor ate o ano anterior ao mais recente aparecem como consolidados.
- O ano mais recente vindo do e-Gestor aparece como preliminar.
- Na base atual, isso resulta em `2021`, `2022`, `2023` e `2024` como consolidados, e `2025` como preliminar.
- Para incluir novos anos no futuro, rode novamente o comando principal. Se a API tiver as 12 competencias do novo ano, o pipeline inclui esse ano automaticamente no Excel e no `.rda`.

## Observacoes

- Feche qualquer instancia do app antes de atualizar os dados para evitar cache travado em `inst/app/data/_rda/`.
- A atualizacao cobre `Municipal` e niveis acima.
- Os niveis abaixo do municipal dentro da cidade de Sao Paulo continuam usando o legado atual do painel.
- O historico de `2020` permanece vindo das planilhas legadas do painel.
