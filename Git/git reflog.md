# Comando `git reflog`

El comando `git reflog` se utiliza para mostrar el historial de movimientos del puntero HEAD y otras referencias en un repositorio de Git. Es útil para recuperar commits perdidos o para ver cambios recientes en las ramas.

## Ejemplo de uso

Para ver el reflog, simplemente ejecuta:

```
git reflog
```

Esto mostrará una lista de los movimientos recientes de HEAD, incluyendo los commits que se han hecho, así como los cambios de rama y otros eventos relevantes.

## Recuperar un commit perdido

Si has perdido un commit y quieres recuperarlo, puedes usar el reflog para encontrar el hash del commit y luego hacer un checkout o un cherry-pick de ese commit.

1. Ejecuta `git reflog` para ver el historial de HEAD.
2. Encuentra el hash del commit que deseas recuperar.
3. Usa `git checkout <hash_del_commit>` para cambiar a ese commit, o `git cherry-pick <hash_del_commit>` para aplicar los cambios de ese commit a tu rama actual.

## Limpiar el reflog

Con el tiempo, el reflog puede crecer mucho. Puedes limpiar entradas antiguas usando:

```
git reflog expire --expire=now --all
git gc --prune=now
```

Esto eliminará las entradas antiguas del reflog y realizará una recolección de basura para liberar espacio.

## Notas adicionales

- El reflog es específico para cada repositorio local y no se comparte con otros repositorios.
- Las entradas del reflog se pueden usar para recuperar commits perdidos, pero no son una solución permanente.
- El reflog se almacena en el directorio `.git/logs/`.
- Por defecto, las entradas del reflog se mantienen durante 90 días, pero esto puede configurarse mediante la opción `gc.reflogExpire` en la configuración de Git.
- El reflog también puede ser útil para deshacer cambios recientes, como resets o merges accidentales.

Ejemplo de lo ultimo mencionado:

```
git reset --hard HEAD@{1}
```

Esto restaurará el estado del repositorio al estado anterior al último cambio de HEAD.

En donde entra el reflog aqui?

El reflog registra todos los movimientos del puntero HEAD, incluyendo resets, checkouts y merges. La sintaxis `HEAD@{n}` se refiere a la posición de HEAD en el reflog, donde `n` es el número de movimientos hacia atrás en el historial del reflog. Por ejemplo, `HEAD@{1}` se refiere al estado de HEAD antes del último cambio.