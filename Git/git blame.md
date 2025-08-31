# Comando `git blame`

El comando `git blame` se utiliza para mostrar la autoría de cada línea en un archivo específico dentro de un repositorio de Git. Proporciona información sobre quién realizó cada cambio, cuándo se realizó y el hash del commit asociado.

## Sintaxis

```bash
git blame [opciones] <archivo>
```

## Uso común

### Mostrar la autoría de un archivo

Para ver la autoría de cada línea en un archivo específico, utiliza:

```bash
git blame nombre_del_archivo
```

### Mostrar la autoría con formato abreviado

Para mostrar la autoría con un formato más compacto, puedes usar la opción `-c`:

```bash
git blame -c nombre_del_archivo
```

### Mostrar la autoría con números de línea

Para incluir números de línea en la salida, utiliza la opción `-l`:

```bash
git blame -l nombre_del_archivo
```

### Mostrar la autoría con fechas

Para incluir las fechas de los commits en la salida, utiliza la opción `-t`:

```bash
git blame -t nombre_del_archivo
```

### Mostrar la autoría para un rango de líneas específico

Para ver la autoría de un rango específico de líneas en un archivo, utiliza la opción `-L` seguida del rango de líneas. Por ejemplo, para ver las líneas 10 a 20:

```bash
git blame -L 10,20 nombre_del_archivo
```

### Mostrar la autoría de un archivo en una rama específica

Para ver la autoría de un archivo en una rama específica, utiliza:

```bash
git blame nombre_del_archivo rama_especifica
```

### Ignorar espacios en blanco

Para ignorar los cambios en espacios en blanco al mostrar la autoría, utiliza la opción `-w`:

```bash
git blame -w nombre_del_archivo
```

### Mostrar la autoría con un formato personalizado
Puedes personalizar la salida del comando `git blame` utilizando la opción `--porcelain`, que proporciona una salida más detallada y estructurada. Por ejemplo:

```bash
git blame --porcelain nombre_del_archivo
```

### Mostrar la autoría de un archivo en un commit específico

Para ver la autoría de un archivo en un commit específico, utiliza el hash del commit:

```bash
git blame <hash_del_commit> -- nombre_del_archivo
```