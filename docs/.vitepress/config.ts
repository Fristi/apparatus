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
          { text: 'Getting Started', link: '/getting-started' },
        ],
      },
      {
        text: 'Modules',
        items: [
          { text: 'Core', link: '/core' },
          { text: 'Doobie', link: '/doobie' },
        ],
      },
    ],
    socialLinks: [
      { icon: 'github', link: 'https://github.com/Fristi/apparatus' },
    ],
  },
})
