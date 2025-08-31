# Comando `git describe`

El comando `git describe` se utiliza para generar una descripción legible de un commit en función de las etiquetas (tags) existentes en el repositorio. Esta descripción incluye el nombre de la etiqueta más cercana, el número de commits desde esa etiqueta y un fragmento del hash del commit.

## Sintaxis

```bash
git describe [opciones] [<commit>]
```


## Resultado típico

El resultado típico de `git describe` tiene el siguiente formato:

```
<tag>-<número_de_commits>-g<hash_del_commit>
```

- `<tag>`: La etiqueta más cercana al commit especificado.
- `<número_de_commits>`: El número de commits realizados desde la etiqueta.
- `<hash_del_commit>`: Un fragmento del hash del commit, precedido por una 'g' (de "git").

El git describe solamente muestra los tags anotados por defecto. Si deseas que también considere los tags ligeros, debes usar la opción `--tags`.

## Uso común

### Describir el commit actual

```bash
git describe
```

### Describir un commit específico

```bash
git describe <commit>
```

### Incluir etiquetas anotadas y ligeras

```bash
git describe --tags
```

### Mostrar solo el nombre de la etiqueta más cercana

```bash
git describe --abbrev=0
```

### Mostrar descripciones completas

```bash
git describe --long
```

### Incluir etiquetas ligeras y mostrar descripciones completas

```bash
git describe --tags --long
```

### Usar un prefijo personalizado para las etiquetas

```bash
git describe --match "v[0-9]*"
```