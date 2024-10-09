# Docker Comandos Básicos - Machete Diario

## Indice

- [Ver contenedores en ejecución](#1-ver-contenedores-en-ejecución)
- [Ver todos los contenedores (incluyendo los detenidos)](#2-ver-todos-los-contenedores-incluyendo-los-detenidos)
- [Iniciar un contenedor detenido](#3-iniciar-un-contenedor-detenido)
- [Detener un contenedor en ejecución](#4-detener-un-contenedor-en-ejecución)
- [Reiniciar un contenedor](#5-reiniciar-un-contenedor)
- [Pausar un contenedor](#6-pausar-un-contenedor)
- [Reanudar un contenedor pausado](#7-reanudar-un-contenedor-pausado)
- [Eliminar un contenedor](#8-eliminar-un-contenedor)
- [Eliminar un contenedor forzosamente](#9-eliminar-un-contenedor-forzosamente)
- [Ver logs de un contenedor](#10-ver-logs-de-un-contenedor)
- [Monitorear el uso de recursos (CPU/Memoria) de los contenedores](#11-monitorear-el-uso-de-recursos-cpumemoria-de-los-contenedores)
- [Crear y ejecutar un nuevo contenedor](#12-crear-y-ejecutar-un-nuevo-contenedor)
- [Listar imágenes de Docker](#13-listar-imágenes-de-docker)
- [Eliminar una imagen de Docker](#14-eliminar-una-imagen-de-docker)

- [Datos del cheat sheet](#datos-del-cheat-sheet)

## 1. Ver contenedores en ejecución
```bash
docker ps
```
Muestra una lista de los contenedores que están corriendo.

## 2. Ver todos los contenedores (incluyendo los detenidos)
```bash
docker ps -a
```
Muestra todos los contenedores, estén en ejecución o no.

## 3. Iniciar un contenedor detenido
```bash
docker start <container_id>
```
Reemplazá `<container_id>` por el ID o nombre del contenedor que querés iniciar.

## 4. Detener un contenedor en ejecución
```bash
docker stop <container_id>
```
Detiene un contenedor en ejecución.

## 5. Reiniciar un contenedor
```bash
docker restart <container_id>
```
Reinicia un contenedor, útil si necesitás aplicar cambios sin eliminarlo.

## 6. Pausar un contenedor
```bash
docker pause <container_id>
```
Suspende temporalmente la ejecución de un contenedor.

## 7. Reanudar un contenedor pausado
```bash
docker unpause <container_id>
```
Reanuda la ejecución de un contenedor pausado.

## 8. Eliminar un contenedor
```bash
docker rm <container_id>
```
Elimina un contenedor que está detenido. Si está en ejecución, debés detenerlo primero.

## 9. Eliminar un contenedor forzosamente
```bash
docker rm -f <container_id>
```
Fuerza la eliminación de un contenedor, incluso si está en ejecución.

## 10. Ver logs de un contenedor
```bash
docker logs <container_id> -f
```
Muestra los logs del contenedor en ejecución. Utilizá la opción `-f` para seguir los logs en tiempo real, especialmente cuando se necesita monitorear el contenedor de manera continua.

## 11. Monitorear el uso de recursos (CPU/Memoria) de los contenedores
```bash
docker stats
```
Muestra el uso de recursos en tiempo real de los contenedores que están en ejecución.

## 12. Crear y ejecutar un nuevo contenedor
```bash
docker run -d --name <nombre> -p <puerto_host>:<puerto_contenedor> <imagen>
```
Crea y ejecuta un nuevo contenedor en segundo plano con la imagen especificada. Utilizá la opción `-p` para mapear puertos entre el host y el contenedor, lo cual es común cuando se necesita acceder a servicios en el contenedor desde el host. Ejemplo:
```bash
docker run -d --name mi-contenedor -p 8080:80 nginx
```

## 13. Listar imágenes de Docker
```bash
docker images
```
Muestra una lista de las imágenes descargadas en tu sistema.

## 14. Eliminar una imagen de Docker
```bash
docker rmi <image_id>
```
Elimina una imagen que ya no necesitás. Si tiene contenedores asociados, debés eliminarlos primero.

### Datos del cheat sheet
- Autor: Nicolás Villamonte <br>
- Fecha: 09/10/2024 <br>
- Email: nicovillamonte@gmail.com <br>
- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
- Herramientas y Versiones: Docker V24.0.0
