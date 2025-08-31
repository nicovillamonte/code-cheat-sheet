# Comando `git reset`

El comando `git reset` se utiliza para deshacer cambios en el repositorio de Git. Permite mover el puntero HEAD a un commit anterior, eliminando los commits posteriores y, opcionalmente, modificando el área de preparación (staging area).

## Sintaxis

```bash
git reset [opciones] [commit]
```

## Uso común

### Deshacer el último commit

Para deshacer el último commit y mantener los cambios en el área de trabajo, utiliza:

```bash
git reset --soft HEAD~1
```

### Deshacer el último commit y eliminar los cambios

Si deseas deshacer el último commit y eliminar los cambios del área de trabajo, utiliza:

```bash
git reset --hard HEAD~1
```

### Deshacer varios commits

Para deshacer varios commits, especifica el número de commits que deseas deshacer. Por ejemplo, para deshacer los últimos 3 commits:

```bash
git reset --soft HEAD~3
```

### Deshacer un commit específico

Si deseas deshacer un commit específico, puedes usar su hash. Por ejemplo:

```bash
git reset --soft <hash_del_commit>
```

### Deshacer cambios en el área de stagging

Si deseas deshacer los cambios en el área de stagging sin afectar el área de trabajo, utiliza:

```bash
git reset HEAD
```

### Deshacer cambios en el área de stagging para un archivo específico

Para deshacer los cambios en el área de stagging para un archivo específico, utiliza:

```bash
git reset HEAD nombre_del_archivo
```

### Deshacer cambios en el área de trabajo

Si deseas deshacer los cambios en el área de trabajo (WD) y el área de stagging, utiliza:

```bash
git reset --hard
```

### Deshacer cambios en un archivo específico

Para deshacer los cambios en un archivo específico en el área de trabajo, utiliza:

```bash
git reset --hard nombre_del_archivo
```