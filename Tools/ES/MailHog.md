# Testeo de envio de mails

Se deberá descargar el archivo del MailHog correspondiente al sistema operativo y arquitectura desde [este repositorio](https://github.com/mailhog/MailHog/blob/master/docs/RELEASES.md). Por ejemplo: _MailHog_windows_386.exe_. 
El mismo se deberá ejecutar para probar el envío mails siguiendo la documentacion del repositorio. Se mostrara en consola dos direcciones ip que marcan un puerto diferente cada una.

![image](https://github.com/nicovillamonte/lapsa-frontend/assets/64659720/cbfb1e62-e643-45f5-b255-1cc1b627d3fc)

El puerto numerado como 1 es el puerto que hay que configurar en el backend que se este desarrollando. El siguiente es un ejemplo desarrollado en _NestJS_ en la configuracion del modulo principal del proyecto.

``` typescript
MailerModule.forRoot({
  transport: {
    host: '0.0.0.0',
    port: 1025,
  },
  defaults: {
    from: 'admin@lapsa.com',
  },
}),
```

Mientras que el puerto numero 2 es el puerto que debemos utilizar para ingresar a la UI en la que vamos a poder ver los mails enviados.

En nuestro caso si es el **8025** entonces vamos a ir a la url **http://localhost:8025**. En la que nos vamos a encontrar una pagina como la siguiente.

![image](https://github.com/nicovillamonte/lapsa-frontend/assets/64659720/b7a1ccb2-64ab-416d-99ef-bad92a37e80c)

Al momento de enviar un mail con la configuaración correcta desde el backend, lo vamos a poder visualizar de la siguiente manera:

![image](https://github.com/nicovillamonte/lapsa-frontend/assets/64659720/79336a68-fc8a-484f-8e25-dfd72fe343a5)

El cual, si abrimos, podemos ver el mail en varios formatos. _HTML_ es el que recibiría el usuario. _Plain Text_ sería el código HTML que resulta el mensaje.
Y el _source _que tiene muchos más datos sobre el envío del mail.

![image](https://github.com/nicovillamonte/lapsa-frontend/assets/64659720/f510c229-195a-4667-85f3-46a184c62835)
