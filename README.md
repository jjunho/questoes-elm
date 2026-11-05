# Questões (Elm)

Aplicação Elm para montar e responder exercícios de língua com tipos de questão distintos: preenchimento de lacuna, classificação, correção de erro, ordenação por arrastar, edição anotada, identificação de gênero e escolha justificada.

## Estrutura

- `src/Domain/` — modelo de questão e tipos
- `src/Layout/` — um layout por tipo de questão
- `src/Data/` — decodificação das questões e amostras
- `plan/questions.md` — notas de concepção

## Uso

```sh
elm make src/Main.elm --output=main.js
```

Abra `index.html` em um servidor estático.
