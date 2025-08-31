# Comando `git config`

El comando `git config` se utiliza para configurar las opciones de Git a nivel global o por repositorio. Permite establecer preferencias como el nombre de usuario, el correo electrónico, el editor predeterminado, entre otros.

## Sintaxis

```bash
git config [opciones] [clave] [valor]
```

## Uso común

### Configurar el nombre de usuario

Para establecer tu nombre de usuario, utiliza:

```bash
git config --global user.name "Tu Nombre"
```

### Configurar el correo electrónico

Para establecer tu correo electrónico, utiliza:

```bash
git config --global user.email "tu_correo@example.com"
```

### Configurar el editor predeterminado

Para cambiar el editor de texto predeterminado que Git utiliza para los mensajes de commit, utiliza:

```bash
git config --global core.editor "nombre_del_editor"
```

### Ver la configuración actual

Para ver todas las configuraciones actuales, utiliza:

```bash
git config --list
```

### Ver una configuración específica

Para ver el valor de una configuración específica, utiliza:

```bash
git config <clave>
```

Por ejemplo, para ver el nombre de usuario:

```bash
git config user.name
```

### Eliminar una configuración

Para eliminar una configuración específica, utiliza:

```bash
git config --unset <clave>
```

Por ejemplo, para eliminar el correo electrónico:

```bash
git config --unset user.email
```

### Configurar alias para comandos

Para crear un alias para un comando de Git, utiliza:

```bash
git config --global alias.<alias> "<comando>"
```

Por ejemplo, para crear un alias que simplifique `git status` a `git st`, utiliza:

```bash
git config --global alias.st status
```

### Configuración a nivel de repositorio

Si deseas establecer una configuración específica para un solo repositorio, omite la opción `--global` y ejecuta los comandos dentro del directorio del repositorio. Por ejemplo:

```bash
git config user.name "Nombre del Repositorio"
```

### Configuración a nivel del sistema

Para establecer una configuración a nivel del sistema (afecta a todos los usuarios en la máquina), utiliza la opción `--system`. Esto generalmente requiere permisos de administrador. Por ejemplo:

```bash
git config --system user.name "Nombre del Sistema"
```