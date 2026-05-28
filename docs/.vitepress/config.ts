import { defineConfig } from "vitepress";
import { withMermaid } from "vitepress-plugin-mermaid";

export default withMermaid(
  defineConfig({
    base: "/apparatus",
    title: "Apparatus",
    description:
      "Scala library for creating basic, eventful and networked state machines",
    themeConfig: {
      nav: [
        { text: "Guide", link: "/getting-started" },
        { text: "GitHub", link: "https://github.com/Fristi/apparatus" },
      ],
      sidebar: [
        {
          text: "Introduction",
          items: [{ text: "What is Apparatus?", link: "/introduction" }],
        },
        {
          text: "Core",
          items: [
            { text: "Getting Started", link: "/core/getting-started" },
            { text: "Mealy machines", link: "/core/machines" },
            { text: "Eventful state machines", link: "/core/decider" },
            { text: "Network of state machines", link: "/core/apparatus" },
            { text: "Creating a Saga", link: "/core/saga" },
            { text: "Isomorphic mapping", link: "/core/iso" },
            { text: "Diagram rendering", link: "/core/mermaid" },
          ],
        },
        {
          text: "Integrations",
          items: [
            { text: "Doobie", link: "/integrations/doobie" }
          ],
        },
        {
          text: "Advanced",
          items: [
            { text: "Higher order fixed point", link: "/advanced/hfix2" }
          ],
        },
      ],
      socialLinks: [
        { icon: "github", link: "https://github.com/Fristi/apparatus" },
      ],
    },
  }),
);
