# Comando `git revert`

El comando `git revert` se utiliza para deshacer cambios en el historial de commits de Git. A diferencia de `git reset`, que elimina commits, `git revert` crea un nuevo commit que revierte los cambios introducidos por un commit anterior.

## Sintaxis

```bash
git revert [opciones] <commit>
```

## Uso común

### Revertir el último commit

Para revertir el último commit, utiliza:

```bash
git revert HEAD
```

### Revertir un commit específico

Si deseas revertir un commit específico, puedes usar su hash. Por ejemplo:

```bash
git revert <hash_del_commit>
```

Si hay conflictos durante el proceso de revert, se deben aceptar los incoming changes y luego hacer un commit manualmente.

### Revertir varios commits

Para revertir varios commits, puedes especificar un rango de commits. Por ejemplo, para revertir los últimos 3 commits:

```bash
git revert HEAD~3..HEAD
```

### Revertir sin abrir el editor

Para revertir un commit sin abrir el editor de mensajes, puedes usar la opción `--no-edit`. Por ejemplo:

```bash
git revert --no-edit HEAD
```