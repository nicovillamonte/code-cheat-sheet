# Los 3 Estados de un Archivo en Git

Git maneja tres estados principales para los archivos en un repositorio:

1. **Modificado (Modified)**: El archivo ha sido cambiado pero aún no se ha guardado en el área de preparación (staging area).
2. **Preparado (Staged)**: El archivo ha sido modificado y se ha añadido al área de preparación, listo para ser confirmado (committed).
3. **Confirmado (Committed)**: El archivo ha sido guardado en el repositorio

## Modified

Un archivo en estado modificado ha sido editado, pero los cambios no han sido añadidos al área de preparación. Para ver los cambios realizados, se puede usar el comando `git diff`.

## Staged

Un archivo en estado preparado ha sido modificado y añadido al área de preparación. Esto significa que los cambios están listos para ser confirmados. Para ver qué archivos están en este estado, se puede usar el comando `git status`.

Al stagear un archivo, se puede proseguir de dos maneras:

1. Confirmar (commitear) los cambos.
2. Seguir modificando el archivo y volver a stagearlo.

También es posible deshacer el stage de un archivo con el comando `git reset`.

## Committed

Un archivo en estado confirmado ha sido guardado en el **repositorio local**. Esto significa que los cambios han sido registrados y se pueden compartir con otros desarrolladores. Para ver el historial de commits, se puede usar el comando `git log`.

## Como pasar de un estado a otro

### Modified a Staged

Se usa el comando `git add` para mover un archivo del estado modificado al estado preparado.

```bash
git add <archivo>
```

### Staged a Committed

Para confirmar los cambios y mover el archivo al estado confirmado, se utiliza el comando `git commit`.

```bash
git commit -m "Mensaje de confirmación"
```

Hasta este punto el archivo ha sido confirmado en el repositorio local. Si se desea compartir estos cambios con un repositorio remoto, se utiliza el comando `git push`.