# Comando `git add`

Cuando un archivo que se encuentra en el Working Directory (WD) ha sido modificado, es necesario agregarlo al área de preparación (staging area) para que los cambios sean incluidos en el próximo commit. Esto se hace con el comando `git add`.

## Uso Básico

1. Abre una terminal o línea de comandos en tu repositorio local.
2. Revisa el estado de tus archivos con el comando:

```bash
git status
```

Esto es totalmente opcional, pero es una buena práctica para ver qué archivos han sido modificados y cuáles están listos para ser confirmados.

3. Ejecuta el siguiente comando:

```bash
git add <archivo>
```

Esto agregará el archivo especificado al área de preparación. Si deseas agregar todos los archivos modificados, puedes usar:

```bash
git add .
```

Otras maneras de realizar esto mismo es con `git add -A` o `git add --all`.

O incluso utilizar regex para agregar archivos específicos:

```bash
git add *.txt
```


Si en este punto ejecutas `git status`, verás que el archivo ha pasado del estado "Modified" a "Staged".