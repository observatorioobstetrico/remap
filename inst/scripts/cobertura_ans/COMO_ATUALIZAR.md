# Atualizacao da Cobertura da Saude Suplementar APS

O fluxo oficial de atualizacao dos dados de Cobertura da Saude Suplementar da
tela de Atencao Primaria a Saude fica em `inst/scripts/cobertura_ans`.

## Comando principal

Rode a partir da raiz do projeto:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/cobertura_ans/update_cobertura_ans.R'
```

## Rebuild sem novo download

Se os arquivos brutos ja estiverem baixados e voce quiser apenas reconstruir
o Excel consolidado e o `.rda`:

```powershell
& 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/cobertura_ans/update_cobertura_ans.R' --rebuild-only
```

## Fonte usada

Cobertura pela saude suplementar, indicador 47a da Matriz SES-SP:

`https://tabnet.saude.sp.gov.br/deftohtm.exe?tabnet/ind47a_matriz.def`

Consulta:

- Linha: `Municipio`
- Coluna: `Nao ativa`
- Conteudos: `Benef saude suplem`, `Populacao total` e `Cobertura SSuple`
- Arquivos: `ans20.dbf` a `ans25.dbf`
- Formato: `prn`
- Opcao marcada: `Exibir linhas zeradas`

## O que o script faz

1. baixa os PRNs do TabNet da SES-SP para:

`inst/app/data/cobertura_ans/raw/`

2. le os arquivos baixados e padroniza municipios
3. junta os municipios com `RRAS-MUNICIPIO.xlsx`
4. calcula a cobertura usada pelo painel:

`(beneficiarios / populacao) * 100`

5. compara a cobertura calculada com a cobertura ja publicada pelo TabNet
6. grava a cobertura calculada como valor final do painel, mantendo tambem a cobertura publicada e a diferenca no Excel para auditoria
7. grava a base documentavel em:

`inst/app/data/cobertura_ans_aps.xlsx`

8. grava a base consumida pelo app em:

`inst/app/data/cobertura_ans_aps.rda`

## Regras de uso no painel

- Os anos 2020 a 2024 entram como consolidados.
- O ano 2025 entra como preliminar.
- O indicador e usado apenas em contextos de municipio ou agregacoes que exibem municipios.
- Contextos por supervisao de saude nao usam esta base, pois o TabNet da SES-SP nao disponibiliza esse indicador nesse nivel.
- O valor exibido no painel prioriza a cobertura calculada a partir de beneficiarios e populacao.

## Inclusao de novos anos

Para incluir novos anos no futuro, ajuste o vetor `years` na chamada de
`build_cobertura_ans_data()` em `cobertura_ans_pipeline.R`, rode o comando
principal e confira o Excel gerado.

## Observacoes

- Feche qualquer instancia do app antes de atualizar os dados para evitar cache
  travado em `inst/app/data/_rda/`.
- O arquivo `metadata_ultima_atualizacao.json` registra a ultima execucao.
- A base nao altera as planilhas `remap*.xlsx`; o app passa a consumir o novo
  `.rda` apenas para os graficos atualizados de Cobertura da Saude Suplementar.
