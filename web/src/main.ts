import { createApp } from "vue";
import "./assets/css/main.css";
import { createRouter, createWebHistory, RouterView } from "vue-router";

const router = createRouter({
	history: createWebHistory(),
	routes: [
		{ path: "/", component: () => import("./app.tsx") },
		{ path: "/dfg", component: () => import("./data_flow_graph.tsx") },
		{ path: "/cft", component: () => import("./control_flow_tree.tsx") },
	],
});

createApp(RouterView).use(router).mount("#app");
