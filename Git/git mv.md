# Comando `git mv`

El comando `git mv` se utiliza para mover o renombrar archivos dentro de un repositorio de Git. Este comando es una combinación de los comandos `mv` del sistema operativo y `git add`, lo que facilita el seguimiento de los cambios en el historial del repositorio.

## Sintaxis

```bash
git mv [opciones] <origen> <destino>
```

## Uso Básico

1. Abre una terminal o línea de comandos en tu repositorio local.
2. Utiliza el comando `git mv` para mover o renombrar un archivo.
3. Confirma los cambios con `git commit`.
4. Si es necesario, sube los cambios al repositorio remoto con `git push`.
5. Asegúrate de que el archivo se haya movido o renombrado correctamente verificando el estado del repositorio con `git status`.

## Ejemplos Comunes

### Renombrar un archivo

Para renombrar un archivo, utiliza el siguiente comando:

```bash
git mv archivo_viejo.txt archivo_nuevo.txt
```

### Mover un archivo a un directorio

Para mover un archivo a un directorio, utiliza:

```bash
git mv archivo.txt directorio/
```

### Mover varios archivos

Para mover varios archivos a un directorio, puedes especificar múltiples archivos:

```bash
git mv archivo1.txt archivo2.txt directorio/
```