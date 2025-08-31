# Comando `git clone`

El comando `git clone` se utiliza para crear una copia local de un repositorio remoto. Este comando descarga todo el historial del proyecto, incluyendo todas las ramas y commits, y configura el repositorio local para que esté vinculado al repositorio remoto.

## Sintaxis

```bash
git clone [opciones] <url_del_repositorio> [directorio_destino]
```

## Https vs SSH

El comando `git clone` puede utilizar tanto URLs HTTPS como SSH para clonar repositorios. La elección entre HTTPS y SSH depende de tus preferencias y del nivel de seguridad que necesites.

- **HTTPS**: Es más fácil de usar para principiantes, ya que solo requiere un nombre de usuario y una contraseña o un token de acceso personal. Sin embargo, puede ser menos seguro si no se utilizan tokens de acceso personal.
- **SSH**: Proporciona una conexión más segura mediante el uso de claves SSH. Requiere una configuración inicial para generar y agregar claves SSH a tu cuenta de Git, pero una vez configurado, permite una autenticación sin contraseña.

### Conexión HTTPS

Para clonar un repositorio utilizando HTTPS, utiliza el siguiente comando:

```bash
git clone https://github.com/usuario/repo.git
```

### Conexión SSH

Para clonar un repositorio utilizando SSH, primero asegúrate de tener configuradas tus claves SSH. Para ello, puedes seguir estos pasos:

#### Verificacion de claves SSH existentes

En Linux o macOS:

```bash
ls ~/.ssh
```

En Windows (PowerShell):

```powershell
dir %USERPROFILE%\.ssh
```

Si ves archivos como `id_rsa` y `id_rsa.pub` o `id_ed25519` y `id_ed25519.pub`, ya tienes claves SSH generadas. Si no, sigue con el siguiente paso para generar una nueva clave SSH.

#### Generacion y configuracion de claves SSH

1. Genera una clave SSH si no tienes una ya:
    ```bash
    ssh-keygen -t ed25519 -C "<tu_correo_electronico>"
    ```

    > Actualmente, se recomienda usar `ed25519` en lugar de `rsa` para una mayor seguridad.

2. Te va a pedir dónde guardar la clave. Presioná Enter para aceptar el path por defecto.

3. Ingresa una frase de contraseña segura cuando se te solicite (opcional pero recomendado).

Esto generará dos archivos: una clave privada (`id_ed25519`) y una clave pública (`id_ed25519.pub`).

#### Agregar la clave SSH al agente SSH

1. Inicia el agente SSH en segundo plano:

    En Linux o macOS:
    ```bash
    eval "$(ssh-agent -s)"
    ```

    En Windows (PowerShell):
    ```powershell
    Start-Service ssh-agent
    ```

2. Configura el agente para que se inicie automáticamente siempre que inicies sesión (solo Windows):

    ```powershell
    Get-Service ssh-agent | Set-Service -StartupType Automatic
    ```

3. Agrega tu clave SSH privada al agente SSH:

    ```bash
    ssh-add ~/.ssh/id_ed25519
    ```


#### Registrar la clave SSH en tu cuenta de GitHub (o la plataforma que uses)

1. Copia el contenido de tu clave pública al portapapeles:

    En Linux o macOS:
    ```bash
    cat ~/.ssh/id_ed25519.pub
    ```

    En Windows (PowerShell):
    ```powershell
    Get-Content $env:USERPROFILE\.ssh\id_ed25519.pub | clip
    ```

2. En GitHub:
     - Settings → SSH and GPG keys → New SSH key
     - Pegá el contenido copiado.

#### Probar la conexión SSH

```bash
ssh -T git@github.com
```

Si es la primera vez que te conectas, es posible que veas un mensaje preguntando si deseas continuar conectándote. Escribe `yes` y presiona Enter. Si todo está configurado correctamente, deberías ver un mensaje de bienvenida.

Si ves un mensaje de error, revisa los pasos anteriores para asegurarte de que todo esté configurado correctamente.

#### Clonar el repositorio usando SSH

Y ya estamos listos para clonar el proyecto utilizando el siguiente comando:

```bash
git clone git@github.com:usuario/repo.git
```

## Uso común

### Clonar un repositorio

Para clonar un repositorio, utiliza el siguiente comando, reemplazando `<url_del_repositorio>` con la URL del repositorio que deseas clonar:

```bash
git clone <url_del_repositorio>
```

### Clonar en un directorio específico

Si deseas clonar el repositorio en un directorio específico, puedes proporcionar el nombre del directorio al final del comando:

```bash
git clone <url_del_repositorio> nombre_del_directorio
```

### Clonar una rama específica

Si deseas clonar una rama específica del repositorio, puedes usar la opción `-b` seguida del nombre de la rama:

```bash
git clone -b nombre_de_rama <url_del_repositorio>
```

### Clonar con profundidad limitada

Para clonar un repositorio con una profundidad limitada (es decir, solo los últimos n commits), utiliza la opción `--depth` seguida del número de commits que deseas clonar:

```bash
git clone --depth n <url_del_repositorio>
```

### Clonar un repositorio usando SSH

Si tienes acceso SSH al repositorio, puedes clonar usando la URL SSH:

```bash
git clone <url_ssh_del_repositorio>
```

### Clonar un repositorio privado

Para clonar un repositorio privado, asegúrate de tener las credenciales adecuadas (como un token de acceso personal o claves SSH) y utiliza la URL correspondiente:

```bash
git clone <url_del_repositorio_privado>
```