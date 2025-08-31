# Comando `git restore`

El comando `git restore` se utiliza para restaurar archivos en el directorio de trabajo a su estado en un commit específico. Es una herramienta útil para descartar cambios no deseados en los archivos.

## Ejemplos de uso

- Para restaurar un archivo específico a su estado en el último commit:

  ```bash
  git restore <nombre_del_archivo>
  ```

- Para restaurar todos los archivos en el directorio de trabajo a su estado en el último commit:

  ```bash
  git restore .
  ```

- Para restaurar un archivo a su estado en un commit específico:

  ```bash
  git restore --source <hash_del_commit> <nombre_del_archivo>
  ```

  o

  ```bash
  git restore -s <hash_del_commit> <nombre_del_archivo>
  ```

- Para restaurar todos los archivos a su estado en un commit específico:

  ```bash
  git restore --source <hash_del_commit> .
  ```