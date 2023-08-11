# Interpretador de JSON de mensajes de GPT Chat API

## Mensajes en formato JSON

En la api de GPT los mensajes se engloban en un arreglo de objetos que contienen un `rol` y un `contenido` haciendo entender que cada uno de ellos es un mensaje en el chat de GPT.

```ts
[
  {
    role: 'system',
    content: 'Contexto del sistema'
  },
  {
    role: 'user',
    content: 'Hola, como andas?'
  },
  {
    role: 'assistant',
    content: '¡Hola! ¡Estoy aquí para ayudarte en lo que necesites! Si tienes alguna pregunta o tema en mente, no dudes en preguntar. Dado que mencionaste que eres un desarrollador fullstack SSR y un investigador universitario, estoy aquí para brindarte información y asistencia relacionada con esos campos. ¿En qué puedo ayudarte hoy?'
  }
]
```

Cada uno de los mensajes tienen un rol asignado:

- **system**: En el contenido del mensaje con este rol se define el contexto del chat para que la IA conteste en base al mismo.
- **user**: Son los mensajes del usuario que esta utilizando el chat.
- **assistant**: Son los mensajes de respuesta del asistente basado en el modelo de IA generativa de texto de [OpenAi](https://openai.com/)

Las conversaciones se basan en el conjunto de estos mensajes y en ocasiones suelen ser persistidas de esta manera en archivos para su posterior utilización o revisión.

## Problema

Las conversaciones muy largas en este formato son dificiles de leer y entender directamente para su revisión o análisis por su aspecto visual poco intuitivo. 
Ya de por sí para leer el ejemplo anterior seguramente tuvo que scrollear mucho para la derecha, lo que no es una buena experiencia. Imaginemosnos como sería si 
el JSON no estuviera bien formateado como el anterior, podríamos encontrarnos con algo así:

```ts
[{role: 'system', content: 'Contexto del sistema'},{role: 'user',content: 'Hola, como andas?'},{role: 'assistant',content: '¡Hola! ¡Estoy aquí para ayudarte en lo que necesites! Si tienes alguna pregunta o tema en mente, no dudes en preguntar. Dado que mencionaste que eres un desarrollador fullstack SSR y un investigador universitario, estoy aquí para brindarte información y asistencia relacionada con esos campos. ¿En qué puedo ayudarte hoy?'}]
```

Nada cómodo de utilizar.


## Solución

En [este repositorio](https://github.com/nicovillamonte/gpt-json-reader) podemos encontrar una aplicación con una interfaz muy sencilla aún en sus primeras versiones, sin embargo suele ser muy útil y sencillo de utilizar, lo que nos va a hacer la vida un poco más sencilla. 
Simplemente ingresando el JSON en el primer texto que se ve en pantalla se va a separar el contexto, mostrarlo en otro TextBox que no puede ser modificado, mientras que el chat entre el usuario y el asistente se va a 
mostrar en un formato más amigable a la vista.

![image](https://github.com/nicovillamonte/gpt-json-reader/assets/64659720/2734d1c9-48c0-4238-b9bb-dc245f87e82d)
