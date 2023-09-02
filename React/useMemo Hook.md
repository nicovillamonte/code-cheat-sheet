# useMemo - Hook de React

El Hook `useMemo` se utiliza para recalcular un valor solo cuando sus dependencias cambian. Es decir, si el valor de las dependencias no cambia, el valor calculado se obtiene de la memoria cache. Es una forma de mejorar el rendimiento de la aplicación.

Vamos a utilizar `React` con `TypeScript` en este ejemplo.

1. [Sintaxis](#sintaxis)
2. [Ejemplo](#ejemplo)
   1. [Usando useState](#usestate-try)
   2. [Problema](#usestate-problema)
   3. [Solución](#usestate-solucion)
3. [A tener en cuenta](#a-tener-en-cuenta)
4. [Más Información](#mas-info)

- [Datos del cheat sheet](#cheat-sheet-data)



<h2 id="sintaxis">Sintaxis</h2>

```ts
const cachedValue = useMemo(calculateValue, dependencies)
```

Tambien ejemplificado en varios sitios como:

```ts
const memoizedValue = useMemo(() => computeExpensiveValue(a, b), [a, b]);
```

El hook `useMemo` recibe dos argumentos:

- `calculateValue`: Función que se ejecutará para calcular el valor que se va a devolver.
- `dependencies`: Array de dependencias que se utilizarán para calcular el valor. Si alguna de las dependencias cambia, se volverá a ejecutar la función `calculateValue` para calcular el nuevo valor. Si no cambia ninguna de las dependencias, se obtendrá el valor de la memoria cache.

<h2 id="ejemplo">Ejemplo</h2>

Vamos a imaginar que tenemos un input de numeros en el que vamos añadiendo numeros a una lista y queremos calcular la suma de todos los numeros de la lista y mostrarlo.

<h3 id="usestate-try">Usando useState</h3>

Lo primero q vamos tender a realizar es a utilizar el hook `useState` para guardar la lista de numeros y con una función que utilice ese estado calcular la suma de todos los numeros de la lista en tiempo real de la siguiente manera:

```tsx
import React, { useState } from 'react';

const Component = () => {
  const [numbers, setNumbers] = useState<number[]>([]);
  const [inputNumber, setInputNumber] = useState<number>(0);

  const handleAddNumber = () => {
    setNumbers([...numbers, inputNumber]);
  };

  const numbersSum = (nums: number[]) => {
    console.log("Calculating sum...");
    return nums.reduce((a, b) => a + b, 0);
  };

  return (
    <div>
      <input
        type="number"
        value={inputNumber ?? ""}
        onChange={(e) => setInputNumber(Number(e.target.value))}
      />
      <button onClick={handleAddNumber}>Add Number</button>
      <span>Sum: {numbersSum(numbers)}</span>
    </div>
  );
};
```

A nivel de lógica tenemos dos estados, uno para guardar la lista de numeros y otro para manejar de manera controlada el input, y dos funciones, una para añadir el numero a la lista y el otro para calcular la suma de todos los numeros de la lista.

A nivel de interfaz tenemos un input que va a actualizar el componente cada vez que se produzca un cambio, un botón para añadir el numero a la lista y un _span_ que va a mostrar la suma de todos los numeros de la lista.


En el _span_ utilizamos la función `numbersSum` para calcular la suma de todos los numeros de la lista con un parámetro: el estado de la lista de numeros. Ésto es lo que va a hacer que cada vez que haya un cambio en el estado de la lista de números se vuelva a ejecutar la función `numbersSum` para calcular la suma de todos los numeros de la lista y renderizar nuevamente el componente con el nuevo valor de la suma.


<h3 id="usestate-problema">Problema</h3>

El problema que tenemos con este ejemplo es que no solamente se va a ejecutar la función `numbersSum` cuando haya un cambio en el estado de la lista de numeros, sino que también se va a ejecutar cuando haya un cambio en el estado del input y con cualquier otro cambio que haga volver a renderizar el componente.

Esto es algo que no queremos que suceda porque la función `numbersSum` va a darnos siempre el mismo resultado mientras no cambie el estado de la lista de numeros.

![](image.png)

Aquí podemos ver que hemos ingresado 4 numeros y la función `numbersSum` se ha ejecutado 4 veces, una por cada vez que se ha actualizado el estado del input. Todos los resultados de esta función fueron cero, porque la lista de numeros no cambió.


<h3 id="usestate-solucion">Solución</h3>

Para solucionar este problema vamos a utilizar el hook `useMemo` para que la función `numbersSum` se ejecute solamente cuando cambie el estado de la lista de numeros.

Por lo tanto, vamos a agregar la línea en la que utilizaremos el hook `useMemo` de la siguiente manera:

```tsx
const sum = useMemo(() => numbersSum(numbers), [numbers]);
```

Ahora tenemos guardado en sum el memoizado de la función `numbersSum` que se va a ejecutar solamente cuando cambie el estado de la lista de numeros ingresada en las dependencias del `useMemo`.

> El `useMemo` solamente recibe como resultado el valor calculado, no la función que lo calcula. Es algo muy importante a tener en cuenta para diferenciarlo con el hook `useCallback` que sí recibiría como resultado la función que lo calcula.

Luego, debemos utilizar el valor de `sum` en el _span_ en vez de utilizar la función `numbersSum` de la siguiente manera:

```tsx
<span>Sum: {sum}</span>
```

Si ahora probamos nuevamente el código, vamos a percatarnos de que cuando escribimos en el input o producimos algún cambio en él, la función `numbersSum` no se ejecuta (Lo sabemos porque no se ejecuta el `conole.log` que tiene en su interior), sino que se ejecuta solamente cuando se modifica la lista de números.

Acá escribimos en el input:

![Alt text](image-1.png)

Acá presionamos el botón que añade el número que se introdujo en el input a la lista de números:

![Alt text](image-2.png)


<h2 id="a-tener-en-cuenta">A tener en cuenta</h2>

El uso del useMemo solamente debe implementarse cuando la ejecución de la función que se memoiza es costosa. Se considera una mala práctica utilizar este Hook cuando la función es muy simple, ya que en ese caso deberíamos tener en cuenta dos cosas: 

- La primera que si ese fuera el caso entonces tendríamos que tener todo el código lleno de `useMemo` y no sería muy legible. 
- Y lo segundo es que hay que tener en cuenta que el uso del `useMemo` también tiene un costo además de que 


<h2 id="mas-info">Más Información</h2>

- [Documentación de React](https://es.react.dev/reference/react/useMemo) para el Hook `useMemo`.
- Otros Artículos
  - [React useMemo hook guide with examples](https://refine.dev/blog/react-usememo/#introduction)
  - [Understanding the React useMemo Hook](https://www.digitalocean.com/community/tutorials/react-usememo)


<br>

<h3 id="cheat-sheet-data">Datos del cheat sheet</h3>

\- Autor: Nicolás Villamonte <br>
\- Fecha: 02/09/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
