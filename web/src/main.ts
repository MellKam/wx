import { createApp } from "vue";
import "./assets/css/main.css";
import { createRouter, createWebHistory, RouterView } from "vue-router";

const router = createRouter({
	history: createWebHistory(),
	routes: [
		{ path: "/", component: () => import("./app.tsx") },
		{ path: "/dfg", component: () => import("./dfg.tsx") },
	],
});

createApp(RouterView).use(router).mount("#app");
