# Comando `git pull`

El comando `git pull` se utiliza para actualizar la rama local con los últimos cambios de la rama remota. Es una combinación de `git fetch` y `git merge`.

## Sintaxis

```bash
git pull [opciones] [repositorio] [rama]
```

## Ejemplo

```bash
git pull origin main
```

Este comando obtiene los cambios de la rama `main` del repositorio remoto llamado `origin` y los fusiona con la rama actual.

Si estamos en la rama `main` y queremos actualizarla con los cambios del repositorio remoto por defecto, simplemente podemos usar:

```bash
git pull
```
