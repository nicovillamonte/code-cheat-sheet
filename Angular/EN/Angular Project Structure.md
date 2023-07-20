# Angular Project Structure

The ideal structure of an Angular project would be organized as follows:

```
├───app
│   ├───core
│   ├───data
│   │   ├───dto
│   │   ├───mocks
│   │   ├───models
│   │   └───services
│   ├───documentation
│   ├───layout
│   ├───modules
│   ├───shared
│   │   ├───components
│   │   └───modules
│   └───styles
└───assets
```

### Core

This module contains classes used by the main application module `app.module`. It includes resources that are always loaded in our application, such as route guards, HTTP interceptors, and application-level services.

```
├───app
│   ├───core
│   │   ├───interceptors
│   │   ├───guards
│   │   └───...
```

### Data

The data module contains the data types (models/entities) and services for data consumed by the application.


```
├───app
│   ├───core
│   ├───data
│   │   ├───dto
│   │   ├───mocks
│   │   ├───models
│   │   └───services
```

### Shared

> This module contains resources used in multiple dynamically loaded modules. By loading them with the application, the shared components are ready when a module requests them.

Within this directory, we can make another classification between modules and components, as we can develop separate components for sharing or create complete modules that can be exported for use in other application modules.

```
├───app
│   ├───shared
│   │   ├───components
│   │   └───modules
```


### Layout

> This directory contains components that act as a layout or are parts of a layout, such as the header, footer, skeleton, etc.

You might find yourself in a dilemma about whether to put the header or footer inside this folder or within the shared folder. Both approaches are valid. However, in this proposed scheme, it is better to develop them within the Layout directory, as this directory specifically focuses on containing components related to the overall structure and design of the application.

### Modules

The modules directory contains a collection of modules that are independent of each other. This allows Angular to load only the module required to display a request, saving bandwidth and accelerating the entire application.

### Documentation

This directory is used to store the project's documentation, which should be in Markdown format (.md) like the one you are currently reading.

While project documentation is generally included in the repository where the code is hosted, this folder can be used for more technical documentation that can be helpful for developers working on the project. Additionally, project guidelines can be noted here for better organization and cleaner, more readable code.

## Source
[Base structure for any Angular project](https://baguilar6174.medium.com/estructura-base-para-cualquier-proyecto-de-angular-6a035a27bfcf) (Spanish)

### Cheat Sheet Information
\- Author: Nicolás Villamonte <br>
\- Date: 20/07/2023 <br>
\- Email: nicovillamonte@gmail.com <br>
\- Linkedin: https://www.linkedin.com/in/nicolasvillamonte/ <br>
