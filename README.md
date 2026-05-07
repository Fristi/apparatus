# apparatus

Scala library to create and compose basic, eventful and network finite state machines (FSM).

Three building blocks

- **Basic** state machines receive input and evolve their state inside, emitting output
- **Eventful** state machines receive input as commands, emit events and the events to evolve the state via the `Decider` pattern
- **Networked** state machines can use basic or eventful state machines to create a network of state machines which can create feedback loops on each other (Saga's) or derive state (Projections) to other parts of the system

The aforementioned is the core. This can be used in a streaming or concurrent setting, where state is volatile and lives in memory.

It becomes interesting when you combine the **Eventful** state machines with a library like doobie, slick or skunk. These libraries allow you to compose transactions via monads. Having a **Network** of **Eventful** _transactional_ statemachines allows you to ingest and persist state and readmodels with strong consistency guarantees within the boundary of one microservice.

Ofcourse you could also use this library to work in an eventual consistent setting 

Full documentation is available at the project website.