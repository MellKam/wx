import { createApp } from "vue";
import { App } from "./App.tsx";
import "./assets/css/main.css";
import { createRouter, createWebHistory, RouterView } from "vue-router";

const router = createRouter({
	history: createWebHistory(),
	routes: [{ path: "/", component: App }],
});

createApp(RouterView).use(router).mount("#app");
