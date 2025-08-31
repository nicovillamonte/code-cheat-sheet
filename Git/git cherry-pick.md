# Comando `git cherry-pick`

El comando `git cherry-pick` se utiliza para aplicar los cambios introducidos por uno o más commits existentes en la rama actual. Esto es útil cuando deseas incorporar cambios específicos de otra rama sin fusionar toda la rama.

## Sintaxis

```bash
git cherry-pick [opciones] <commit>
```

## Cuando usarlo

El comando `git cherry-pick` es útil en situaciones donde deseas aplicar cambios específicos de una rama a otra sin fusionar toda la rama. Algunos casos comunes incluyen:

- Aplicar correcciones de errores desde una rama de desarrollo a una rama de producción.
- Incorporar características específicas de una rama experimental a la rama principal sin fusionar todos los cambios experimentales.
- Seleccionar commits específicos para incluir en una rama de lanzamiento.
- Revertir cambios específicos en la rama actual aplicando un commit inverso.


> Importante: Este comando puede generar conflictos si los cambios en el commit que estás aplicando entran en conflicto con los cambios en la rama actual. En caso de conflicto, Git pausará el proceso de cherry-pick para que puedas resolver los conflictos manualmente.


## Uso común

### Aplicar un commit específico

Para aplicar un commit específico a la rama actual, utiliza el hash del commit:

```bash
git cherry-pick <hash_del_commit>
```

### Aplicar varios commits

Puedes aplicar varios commits especificando un rango de commits. Por ejemplo, para aplicar los últimos 3 commits:

```bash
git cherry-pick HEAD~3..HEAD
```

### Aplicar commits no consecutivos

Para aplicar commits no consecutivos, puedes listar los hashes de los commits separados por espacios:

```bash
git cherry-pick <hash_commit1> <hash_commit2> <hash_commit3>
```

### Omitir un commit con conflictos

Si encuentras conflictos durante el cherry-pick y deseas omitir el commit problemático, puedes usar la opción `--skip` después de resolver los conflictos:

```bash
git cherry-pick --skip
```

### Revertir un cherry-pick

Si deseas revertir un cherry-pick que ya has aplicado, puedes usar el comando `git revert` con el hash del commit resultante del cherry-pick:

```bash
git revert <hash_del_commit_cherry_pick>
```

### Continuar un cherry-pick pausado

Si el cherry-pick se pausa debido a conflictos y has resuelto los conflictos, puedes continuar el proceso con:

```bash
git cherry-pick --continue
```

### Abort a cherry-pick

Si deseas abortar un cherry-pick en curso y volver al estado anterior, puedes usar:

```bash
git cherry-pick --abort
```