# useEffect - Hook de React

El hook `useEffect` se encarga de invocar código como efecto secundario de un cambio. Es decir, se ejecuta cada vez que se actualiza el estado de un componente.

Vamos a utilizar `React` con `TypeScript` en este ejemplo.

1. [Sintaxis](#sintaxis)
2. [Ejecutar código al renderizar el componente](#ejecutar-código-al-renderizar-el-componente)
3. [Ejecutar código cada vez que se actualiza el estado](#ejecutar-código-cada-vez-que-se-actualiza-el-estado)
4. [Ejecutar código cuando se desrenderiza el componente](#ejecutar-código-cuando-se-desrenderiza-el-componente)
5. [Casos de uso](#casos-de-uso)
6. [Más Información](#más-información)

- [Datos del Cheat Sheet](#datos-del-cheat-sheet)

## Sintaxis

```ts
useEffect(() => {
    // Código a ejecutar
}, [dependencias]);
```

El `useEffect` recibe como **primer argumento** la función que se quiere ejecutar, en la que se econtrarán las operaciones que se ejecutarán de manera colateral a un cambio. 

Como **segundo argumento** recibe un arreglo de dependencias, que son las variables que determinarán, mediante cambios, cuándo se ejecutará la función que se pasa como primer argumento.

## Ejecutar código al renderizar el componente

Si el arreglo de dependencias está vacío, la función se ejecutará una sola vez, cuando el componente se renderice.

```tsx
useEffect(() => {
    console.log("El componente se renderizó");
}, []);
```

Si ejecutamos el código anterior, veremos que en la consola se imprime el mensaje una sola vez, cuando el componente se renderiza.

## Ejecutar código cada vez que se actualiza el estado

Si el arreglo de dependencias contiene estados, la función se ejecutará cada vez que alguna de ellos reciba un cambio mediante su función de setter.

```tsx
const [name, setName] = useState("");

useEffect(() => {
    console.log("El nombre cambió");
}, [name]);
```

Cada vez que el nombre cambie utilizando la función `setName`, se ejecutará la función que se pasa como primer argumento al `useEffect`.

Pueden definirse varios estados en el arreglo de dependencias, separados por comas.

```tsx
const [name, setName] = useState("");
const [age, setAge] = useState(0);

useEffect(() => {
    console.log("El nombre o la edad cambiaron");
}, [name, age]);
```

## Ejecutar código cuando se desrenderiza el componente

Si la función que se pasa como primer argumento al `useEffect` devuelve una función, esta se ejecutará cuando el componente se desrenderice o destruya.

```tsx
useEffect(() => {
    console.log("El componente se renderizó");

    return () => {
        console.log("El componente se desrenderizó");
    };
}, []);
```

Si lo ejecutamos, veremos que en la consola se imprime el mensaje una sola vez, cuando el componente se renderiza, y luego se imprime el mensaje cuando el componente se desrenderiza al cambiar de página por ejemplo.


## Casos de uso

- **Ejecutar código al renderizar el componente**: Se puede utilizar por ejemplo para hacer un llamado a una API y obtener datos que se utilizarán en el componente.

    ```tsx
    const [users, setUsers] = useState<User[]>([]);

    useEffect(() => {
        fetch("https://jsonplaceholder.typicode.com/users")
            .then((response) => response.json())
            .then((data) => {
                setUsers(data);
            });
    }, []);
    ```

- **Ejecutar código cada vez que se actualiza el estado**

    ```tsx
    const [carrito, updateCarrito] = useState<Producto[]>([]);
    const [total, setTotal] = useState(0);

    useEffect(() => {
        let total = 0;

        carrito.forEach((producto) => {
            total += producto.precio;
        });

        setTotal(total);
    }, [carrito]);
    ```

- **Ejecutar código cuando se desrenderiza el componente**: Se puede utilizar por ejemplo para limpiar un intervalo de tiempo que se haya creado.

    ```tsx
    useEffect(() => {
        const interval = setInterval(() => {
            console.log("Hola");
        }, 1000);

        return () => {
            clearInterval(interval);
        };
    }, []);
    ```

## Más Información

- Se puede ver la [documentación de React](https://react.dev/reference/react/useEffect) sobre el Hook `useEffect`.
- Otra fuente más compoleta. [A complete guide to the useEffect React Hook](https://blog.logrocket.com/useeffect-hook-complete-guide)
- Otros hooks pueden verse en este mismo repositorio. [Code Cheat Sheet - React](https://github.com/nicovillamonte/code-cheat-sheet/tree/main/React)


<br>

### Datos del Cheat Sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 31/08/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
\- Herramientas y Versiones: React 18.2.0, Nodejs 18.14.0