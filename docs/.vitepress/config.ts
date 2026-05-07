import { defineConfig } from 'vitepress'

export default defineConfig({
  base: '/apparatus',
  title: 'Apparatus',
  description: 'Scala toolkit for tagless final algebras with FSM-driven schema and database bindings',
  themeConfig: {
    nav: [
      { text: 'Guide', link: '/getting-started' },
      { text: 'GitHub', link: 'https://github.com/Fristi/apparatus' },
    ],
    logo: '/logo.jpg',
    sidebar: [
      {
        text: 'Introduction',
        items: [
          { text: 'What is Apparatus?', link: '/introduction' },
        ],
      },
      {
        text: 'Core of Apparatus',
        items: [
          { text: 'Getting Started', link: '/core/getting-started' },
          { text: 'Basic state machines', link: '/core/base-machine' },
          { text: 'Eventful state machines', link: '/core/decider' },
          { text: 'Network of state machines', link: '/core/fsm' },
          { text: 'Isomorphic mapping', link: '/core/fsm' },
        ],
      }
    ],
    socialLinks: [
      { icon: 'github', link: 'https://github.com/Fristi/apparatus' },
    ],
  },
})
