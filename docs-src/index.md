---
layout: home

hero:
  name: "Apparatus"
  text: "Composable FSM forests"
  tagline: General purpose FSM forests via (non) persistent FSM
  image:
    src: "/logo.jpg"
    alt: Apparatus
  actions:
    - theme: brand
      text: Get Started
      link: /core/getting-started
    - theme: alt
      text: Introduction
      link: /introduction
    - theme: alt
      text: GitHub
      link: https://github.com/Fristi/apparatus

features:
  - title: General purpose
    details: Can be used for (non) persistent FSM in stream processing or concurrent programming
  - title: Event sourcing
    details: Define aggregates with `Decider` and project the output read-models with ease
  - title: Sagas
    details: Easily create composable saga networks
  - title: Doobie Integration
    details: Doobie support for persistent `Decider` via `ConnectionIO`, can be composed with projections to achieve *strongly consistent* read-models
---
