# useState - Hook de React

El Hook de React `useState` te permite añadir _reactividad_ a componentes funcionales mediante estados. El componente se vuelve a renderizar cada vez que el estado cambia.

Vamos a utilizar `React` con `TypeScript` en este ejemplo.

## Sintaxis

```ts
const [state, setState] = useState<type>(initialState);
```

El useState recibe como argumento el valor inicial del estado y devuelve un arreglo con dos elementos, un `getter` y un `setter` a los que podemos llamarlos de cualquier forma. Por convención se suelen llamar al getter con el nombre de lo que estamos almacenando (ej. `email`) y al setter con el prefijo _set_ seguido del nombre del estado (ej. `setEmail`).

El tipo de dato que se le pasa a `useState` es opcional y solo es posible utilizarlo en TypeScript. Es recomendable utilizarlo cuando el tipo de dato con el valor que se inicializa no se puede inferir automáticamente.

En el siguiente ejemplo, el tipo de dato se infiere automáticamente:

```ts
const [name, setName] = useState("Fernando");
```

Mientras que en el siguiente, se especifica el tipo de dato porque no se puede entender solo por el valor inicial que es un objeto de tipo `User`:

```ts
const [user, updateUser] = useState<User>({
    name: "Fernando",
    age: 40,
});
```

Tambien puede utilizarse cuando se inicializa con un valor `null`:

```ts
const [user, updateUser] = useState<User | null>(null);
```

## Utilización del estado

Se puede utilizar el estado en cualquier parte del componente, acá se muestran las formas en las que más se suelen utilizar los estados.

### Obtener el valor del estado

```tsx
const [name, setName] = useState("");

return <h1>{name}</h1>;
```

En este caso se mostraría como título el valor del estado `name`. Es un ejemplo simple que hay que entender para pasar al siguiente.

### Actualizar el valor del estado

```tsx
const [name, setName] = useState("");

const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setName(e.target.value);
};

return <>
    <input type="text" value={name} onChange={handleChange} />
    <span>{name}</span>
</>;
``` 

En este caso se muestra un input. El input contiene el valor del estado name constantemente actualizado, mientras que él mismo es el que lo actualiza con el evento `onChange`, que llama a una función que actualiza el estado con el valor del input, lo que es reflejado tanto en el valor del mismo input como en el span que muestra el valor del estado.

## Actualizando objetos y arreglos

Los estados pueden ser de cualquier tipo de dato, incluyendo objetos y arreglos, los cuales se entiende que son mutables. Mientras que el estado en React se debe mantener inmutable, se puede actualizar el estado de un objeto o arreglo de la siguiente manera:

```ts
const [user, updateUser] = useState<User>({
    name: "Fernando",
    age: 40,
});

const handleClick = (e: React.ChangeEvent<HTMLInputElement>) => {
    updateUser({
        ...user,
        age: e.target.value,
    });
};
```

De esta manera, se crea un nuevo objeto con el operador _spread_ y se actualiza el valor del estado con el nuevo objeto.

Lo mismo para los arreglos:

```ts
const [array, setArray] = useState<number[]>([1,2]);

const handleClick = (newItem: number) => {
    setArray([
        ...array,
        newItem,
    ]);
};
```

## Actualizando el estado basandose en el estado anterior

Cuando se actualiza el estado, se puede utilizar el estado anterior para actualizarlo. Esto es muy útil en ciertas ocaciones que se quiera actualizar el estado dos veces seguidas por ejemplo, ya que el estado se actualiza de manera asíncrona.

```ts
const [count, setCount] = useState(0);

const handleClick = () => {
    setCount(count + 1);
    setCount(count + 1);
};
```

En este caso, la primera vez que se haga click en el botón, el estado se actualizará a 1, pero la segunda vez que se haga click, se actualizará a 1 nuevamente, ya que, al ser asíncrónico, el valor de `count` no se actualiza en el momento que se llama al segundo `setCount` y en ambos casos se utiliza el valor cero para sumarle uno. Para solucionar esto, se puede utilizar el estado anterior para actualizarlo:

```ts
const [count, setCount] = useState(0);

const handleClick = () => {
    setCount(prev => prev + 1);
    setCount(prev => prev + 1);
};
```

En este caso se actualizará a 2 con tan solo el primer click, ya que el segundo `setCount` se actualizará con el valor del estado anterior, que en este caso es 1.

## Más Información

Se puede ver la documentación de React sobre el Hook `useState` [acá](https://legacy.reactjs.org/docs/hooks-reference.html#usestate).

El hook useState suele utilizarse bastante junto con el hook `useEffect`. (Proximamente habrá un cheat sheet).