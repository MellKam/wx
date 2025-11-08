import "./global.css";
import { render } from "solid-js/web";
import { Router, type RouteDefinition } from "@solidjs/router";
import { lazy } from "solid-js";

const routes = [
	{
		path: "/",
		component: lazy(() => import("./pages/index.tsx")),
	},
] satisfies RouteDefinition[];

render(() => <Router>{routes}</Router>, document.getElementById("app")!);
