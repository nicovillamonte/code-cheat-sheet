# useTransition - Hook de React

El hook `useTransition` nos permite manejar la transición de un estado a otro en React, asignandole menor prioridad a la actualización de estados cuyo renderizado puede ser costoso y no es necesario que se reflejen inmediatamente en la interfaz.

1. [Problema](#problema)
2. [Sintaxis](#sintaxis)
3. [Implementación](#implementacion)

- [Datos del cheat sheet](#cheat-sheet-data)


<h2 id="problema">Problema</h2>

Vamos a usar un ejemplo didáctico para entender el problema, el mismo consta de un `input` que, al escribir en él, se renderiza debajo del mismo su valor miles de veces.

```tsx
function App() {
  const [inputValue, setInputValue] = useState("")
  const [list, setList] = useState<string[]>([]);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInputValue(e.target.value);

    //Simulamos un renderizado costoso
    const newList = [];
    for(let i = 0; i < 20000; i++) {
      newList.push(e.target.value)
    }
    setList(newList)
  }

  return (
    <div>
      <input type="text" value={inputValue} onChange={handleChange} />
      
      {list.map((item, index) => (
        <p key={index}>{item}</p>
      ))}
    </div>
  );
}
```

En este ejemplo, al escribir en el `input`, se renderiza debajo del mismo su valor miles de veces, lo que va a relentizar la aplicación cada vez que se haga un cambio en el mismo.

Esto se debe a que, por defecto, todos los estados tienen una alta prioridad a la hora de ser actualizados. Sin embargo, en este caso no es tan importante que le de prioridad a la actualización del estado `list` ya que no es necesario que se refleje inmediatamente en la interfaz, pero sí es importante que se actualice el estado `inputValue` para que el usuario pueda seguir escribiendo sin que se trabe toda la aplicación.

Para solucionar este problema, podemos utilizar el hook `useTransition`.

<h2 id="sintaxis">Sintaxis</h2>

```tsx
const [isPending, startTransition] = useTransition();
```

El hook `useTransition` devuelve un array con dos elementos:

- `isPending`: booleano que indica si la transición está pendiente.
- `startTransition`: función que inicia la transición.


<h2 id="implementacion">Implementación</h2>

Vamos a implementar este hook en el ejemplo anterior para solucionar el problema que estabamos teniendo.

Para ello, vamos a encerrar los procesos costoso en la función que nos devolvió el hook `useTransition` para comenzar la transición y, de esta manera, darle menor prioridad a la actualización del estado `list`.

```tsx
const [isPending, startTransition] = useTransition();

const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setInputValue(e.target.value);

    startTransition(() => {
        const newList = [];
        for (let i = 0; i < 20000; i++) { // Simulamos una carga lenta
        newList.push(e.target.value);
        }
        setList(newList);
    });
};
```

Como vemos, la actualización del valor del input no fue incluída en la función `startTransition`, por lo que se va a seguir actualizando inmediatamente con alta prioridad. Mientras que la actualización del estado `list` se va a dar con menor prioridad, por lo que no va a relentizar la aplicación.

Ahora tenemos, gracias al hook, una variable `isPending` que nos indica si la transición está pendiente. Por lo que podemos utilizarla para mostrar un mensaje de carga mientras se está realizando la transición.

El HTML del componente quedaría de la siguiente manera:

```tsx
return (
    <div>
        <input type="text" value={inputValue} onChange={handleChange} />

        {isPending ? (
            <p>Loading...</p>
        ) : (
            <>
                {list.map((item, index) => (
                    <p key={index}>{item}</p>
                ))}
            </>
        )}
    </div>
);
```

Si probamos el código de esta manera, entonces vamos a ver que se puede escribir en el `input` sin que se trabe la aplicación, mientras que se muestra el mensaje de carga _Loading..._ hasta que se termine de actualizar el estado `list`, momento en el que se va a renderizar el mismo sin problema.

> Se puede aumentar la cantidad de elementos que se agregan al estado `list` para ver mejor el funcionamiento del hook si es necesario. Solo hay que cambiar el valor de finalización del `for` en la función `handleChange`.

Es importante entender que el hook `useTransition` no hace que la transición sea más rápida, sino que le da menor prioridad a la actualización del estado, por lo que no va a relentizar las partes prioritarias de la aplicación.


<h2 id="mas-info">Más Información</h2>

- [Documentación Oficial de React](https://react.dev/reference/react/useTransition) del hook `useTransition`.
- [Cheat Sheet sobre el hook `useState`](https://github.com/nicovillamonte/code-cheat-sheet/blob/main/React/useState%20Hook.md)


<br>

<h3 id="cheat-sheet-data">Datos del cheat sheet</h3>

\- Autor: Nicolás Villamonte <br>
\- Fecha: 09/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>