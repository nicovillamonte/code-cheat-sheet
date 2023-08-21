# Serialización y Deserialización con el patrón de diseño Strategy

La serialización y deserialización no es algo difícil de lograr la mayoría de las veces, ya que se suele tener que crear una instancia de un objeto o interfaz con los mismos valores del que se quiere serializar o deserializar.

Sin embargo, cuando se utiliza el patrón de diseño Strategy, la serialización y deserialización se vuelve un poco más complicada, ya que dentro del strategy se esconde el tipo de objeto que se está utilizando.

> Se puede ver aplicada una solución en el [ejemplo de serialización con Strategy en Kotlin](https://github.com/nicovillamonte/eg-strategy-serialization-kotlin) para la materia Algoritmos 2 de la [Universidad de San Martin (UNSAM)](https://www.unsam.edu.ar/).

## Ejemplo

Imaginemos que tenemos una interfaz que representa a los vehículos en general, la misma es implementada por las clases `Auto`, `Moto` y `Bicicleta`. Mientras tanto, tenemos una base de datos que guarda todos los vehículos en una tabla con el patrón de Single Table, es decir, todos los vehículos se guardan en la misma tabla, y se identifican por un campo `tipo` que indica el tipo de vehículo que es.

En este caso, al serializar un vehículo, debemos guardar el tipo de vehículo que es, y al deserializarlo, debemos crear una instancia del tipo de vehículo que es realmente, sino nuestro programa no funcionará correctamente.

Nuestro desafío es encontrar una manera fácil y rápida de serializar y deserializar en este tipo de ejemplos, sin tener que hacer un `if` por cada tipo de vehículo que existe, lo cual es una muy mala práctica.

## Solución

### Modelo

Imaginemos entonces que tenemos el siguiente diagrama de clases para el strategy de vehículos:

<p align="center">
    <img src="https://github.com/nicovillamonte/code-cheat-sheet/assets/64659720/ca8dc623-cc65-46e4-a335-031554cffad5" />
</p>


Como se puede ver, tenemos una interfaz `Vehiculo` que es implementada por las clases `Auto`, `Moto` y `Bicicleta`. El ejemplo es muy sencillo y los fines del mismo no son explicar el patrón Strategy, sino explicar como serializar y deserializar en casos en los que se utilice éste patrón u otro que maneje varias tipos de clases que implementan o heredan de otras.

Vamos a tener que crear un DTO (Data Transfer Object) general para todos los vehículos, que contenga todos los campos que se quieren serializar y deserializar, y además, un campo `tipo` que indique el tipo de vehículo que es.

``` kotlin
data class VehiculoDTO(
    var marca: String,
    var modelo: String,
    var anio: String,
    var tipo: String
) {}
```

### Mapa de los tipos de vehículos

Y acá es donde comienza la magia con la que vamos a poder salvarnos de una cascada de if's enorme. Vamos a crear un mapa en el que se contenga en la key el tipo de vehículo en formato de texto (String) y en el value la clase a la que hace referencia ese tipo de vehículo, lo que se puede realizar utilizando el operador `::` de Kotlin seguido de la palabra clave `class`.

``` kotlin
val VehiculoMap: Map<String, KClass<out Vehiculo>> = mapOf(
    "Auto" to Auto::class,
    "Moto" to Moto::class,
    "Bicicleta" to Bicicleta::class
)
```

Como se puede ver, estamos haciendo referencia a un mapa que tiene como key un String y como value una clase que implementa la interfaz `Vehiculo`. Esto es posible gracias a la palabra clave `out` que se utiliza en la definición del mapa, y que permite que el value del mapa sea una clase que implemente la interfaz `Vehiculo`. Con esto, cada vez que se añada un nuevo tipo de vehículo, se deberá agregar una nueva key y value al mapa, y no se deberá modificar nada más.

### Serialización

En un archivo a parte, por ejemplo `VehiculosSerializador.kt`, vamos a crear un método para la interfaz de Vehiculo para serializarlo a un DTO. Para ello vamos a serializar como serializamos normalmente, con la diferencia de que vamos a agregar el tipo de vehículo al DTO y vamos a utilizar el mapa creado anteriormente para asignarle su valor de una manera más dinámica.

``` kotlin
fun Vehiculo.toDto(): VehiculoDTO {
    return VehiculoDTO(
        marca = this.marca,
        modelo = this.modelo,
        anio = this.anio,
        tipo = VehiculoMap.filterValues { it == this::class }.keys.first()
    )
}
```

Con esto, solamente deberíamos de llamar a este método para serializar un vehículo a un DTO, y no deberíamos de preocuparnos por el tipo de vehículo que es. Por ejemplo:

``` kotlin
val auto = Auto("Ford", "Fiesta", "2019")
val autoDTO = auto.toDto()

println(autoDTO)
```

Veremos como resultado:

``` kotlin
VehiculoDTO(marca=Ford, modelo=Fiesta, anio=2019, tipo=Auto)
```

### Deserialización

Ahora vamos a crear un método para deserializar un DTO a un vehículo. Para ello vamos a utilizar el mapa creado anteriormente para obtener la clase del vehículo que se quiere deserializar según el tipo de vehículo que se haya guardado en el DTO y crear una instancia de la misma con los valores del DTO.

``` kotlin
fun vehiculoFromDto(vehiculoDto: VehiculoDTO): Vehiculo? {
    val tipo: KClass<out Vehiculo> = VehiculoMap[vehiculoDto.tipo]!!
    val constructor = tipo.constructors.firstOrNull()

    return constructor?.callBy(mapOf(
        constructor.parameters[0] to vehiculoDto.marca,
        constructor.parameters[1] to vehiculoDto.modelo,
    )).also {
        it?.anio = vehiculoDto.anio
    }
}
```

Como vemos, los parametros deben de estar en el mismo orden que los parametros del constructor de la clase, y además, se puede utilizar el `.also` o `.apply` para asignarle otros valores que no se piden en el constructor.

En caso de no necesitar pasar ningun argumento se debe pasar el mismo mapa vacío, por ejemplo:

``` kotlin
fun vehiculoFromDto(vehiculoDto: VehiculoDTO): Vehiculo? {
    val tipo: KClass<out Vehiculo> = VehiculoMap[vehiculoDto.tipo]!!
    val constructor = tipo.constructors.firstOrNull()

    return constructor?.callBy(mapOf())
}
```

## Otras cosas a tener en cuenta

Esto es solamente una manera muy simplificada de manejar la serialización y deserialización con el patrón de diseño Strategy o parecidos en el que se manejen tipos, implementación o herencia. Sin embargo, es recomendable agregarle más validaciones y manejo de errores para que sea más robusto. Además, se puede realizar la función que creamos para deserializar como un método estático de la interfaz `Vehiculo` para que sea más legible y fácil de utilizar.

Por lo que esto es solamente la explicación de cómo se puede llegar a realizar, sin embargo tiene muchas cosas que se pueden mejorar y agregar para que sea más robusto y fácil de utilizar.


### Datos del cheat sheet

\- Autor: Nicolás Villamonte <br>
\- Fecha: 22/08/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
