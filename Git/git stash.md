# Comando `git stash`

El comando `git stash` se utiliza para guardar temporalmente los cambios en el área de trabajo y el área de preparación (staging area) sin hacer un commit. Esto es útil cuando necesitas cambiar de rama o realizar otras tareas sin perder los cambios actuales.

## Sintaxis

```bash
git stash [opciones] [mensaje]
```

## Uso común

### Guardar cambios

Para guardar los cambios actuales en el stash, utiliza:

```bash
git stash
```

### Guardar cambios con un mensaje

Si deseas agregar un mensaje descriptivo al stash, puedes usar:

```bash
git stash save "mensaje descriptivo"
```

### Listar stashes

Para ver una lista de los stashes guardados, utiliza:

```bash
git stash list
```

### Aplicar el último stash

Para aplicar el último stash guardado, utiliza:

```bash
git stash apply
```

### Aplicar un stash específico

Si deseas aplicar un stash específico, puedes usar su nombre. Por ejemplo:

```bash
git stash apply stash@{0}
```

### Eliminar el último stash

Para eliminar el último stash guardado, utiliza:

```bash
git stash drop
```

### Eliminar un stash específico

Para eliminar un stash específico, puedes usar su nombre. Por ejemplo:

```bash
git stash drop stash@{0}
```

### Limpiar todos los stashes

Para eliminar todos los stashes guardados, utiliza:

```bash
git stash clear
```

### Aplicar y eliminar el último stash

Si deseas aplicar el último stash y eliminarlo al mismo tiempo, utiliza:

```bash
git stash pop
```

### Aplicar y eliminar un stash específico

Para aplicar y eliminar un stash específico, puedes usar su nombre. Por ejemplo:

```bash
git stash pop stash@{0}
```


### Aplicar un stash solo de archivos unstageados

Para aplicar un stash que solo contiene archivos unstageados, utiliza la opción `--keep-index` o `-k`:

```bash
git stash push --keep-index
```

Es recomendable dejarlo con un mensaje descriptivo:

```bash
git stash push -k -m "mensaje descriptivo"
```