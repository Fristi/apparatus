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
          text: "Core of Apparatus",
          items: [
            { text: "Getting Started", link: "/core/getting-started" },
            { text: "Basic state machines", link: "/core/base-machine" },
            { text: "Eventful state machines", link: "/core/decider" },
            { text: "Network of state machines", link: "/core/fsm" },
            { text: "Creating a Saga", link: "/core/saga" },
            { text: "Isomorphic mapping", link: "/core/iso" },
            { text: "Diagram rendering", link: "/core/mermaid" },
          ],
        },
      ],
      socialLinks: [
        { icon: "github", link: "https://github.com/Fristi/apparatus" },
      ],
    },
  }),
);
