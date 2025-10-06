import * as d3 from "d3";
import { defineComponent, ref, onMounted, watch, type PropType } from "vue";

type DFGNodeKind =
	| {
			type: "ConstInt";
			value: number;
	  }
	| {
			type: "Parameter";
			index: number;
	  }
	| {
			type: "Add";
			left: number;
			right: number;
	  }
	| {
			type: "Sub";
			left: number;
			right: number;
	  }
	| {
			type: "Mul";
			left: number;
			right: number;
	  }
	| {
			type: "Div";
			left: number;
			right: number;
	  };

interface DFGNode {
	kind: DFGNodeKind;
	uses: number[];
}

const createD3Nodes = (graph: DFGNode[]) => {
	return graph.map((node, index) => {
		const { kind } = node;
		let type: "parameter" | "const" | "operation";
		let label: string;

		switch (kind.type) {
			case "ConstInt":
				type = "const";
				label = kind.value.toString();
				break;
			case "Parameter":
				type = "parameter";
				label = kind.index.toString();
				break;
			case "Add":
				type = "operation";
				label = "ADD";
				break;
			case "Sub":
				type = "operation";
				label = "SUB";
				break;
			case "Mul":
				type = "operation";
				label = "MUL";
				break;
			case "Div":
				type = "operation";
				label = "DIV";
				break;
		}

		return {
			id: index,
			type,
			label,
		} as d3.SimulationNodeDatum & {
			id: number;
			type: "parameter" | "const" | "operation";
			label: string;
		};
	});
};

const createD3Links = (graph: DFGNode[]) => {
	const links: (d3.SimulationLinkDatum<d3.SimulationNodeDatum> & {
		type: "operand";
	})[] = [];
	for (let index = 0; index < graph.length; index++) {
		const kind = graph[index]!.kind;

		if (
			kind.type === "Add" ||
			kind.type === "Sub" ||
			kind.type === "Mul" ||
			kind.type === "Div"
		) {
			links.push({
				source: kind.left,
				target: index,
				type: "operand",
			});
			links.push({
				source: kind.right,
				target: index,
				type: "operand",
			});
		}
	}

	return links;
};

const DFGGraph = defineComponent({
	props: {
		graph: {
			type: Array as PropType<DFGNode[]>,
			required: true,
		},
		width: {
			type: Number,
			required: true,
		},
		height: {
			type: Number,
			required: true,
		},
	},
	setup(props) {
		const svgRef = ref<SVGSVGElement>();

		const colors = {
			parameter: "#388E3C",
			const: "#1976D2",
			operation: "#E64A19",
		};

		const createVisualization = () => {
			if (!svgRef.value) return;
			d3.select(svgRef.value).selectAll("*").remove();

			const nodes = createD3Nodes(props.graph);
			const links = createD3Links(props.graph);
			const svg = d3.select(svgRef.value);

			// Create defs for patterns and markers
			const defs = svg.append("defs");

			// Create grid pattern
			const gridPattern = defs
				.append("pattern")
				.attr("id", "grid")
				.attr("width", 48)
				.attr("height", 48)
				.attr("patternUnits", "userSpaceOnUse");

			gridPattern
				.append("path")
				.attr("d", "M 50 0 L 0 0 0 50")
				.attr("fill", "none")
				.attr("stroke", "white")
				.attr("stroke-width", 1)
				.attr("opacity", 0.15);

			// Add arrow marker to defs
			defs
				.append("marker")
				.attr("id", "arrow")
				.attr("viewBox", "0 -5 10 10")
				.attr("refX", 30)
				.attr("refY", 0)
				.attr("markerWidth", 6)
				.attr("markerHeight", 6)
				.attr("orient", "auto")
				.append("path")
				.attr("d", "M0,-5L10,0L0,5")
				.attr("fill", "#cccccc");

			// Create a container group for zoom/pan
			const container = svg.append("g").attr("class", "zoom-container");

			// Add grid background to the container (so it moves with zoom/pan)
			const gridRect = container.append("rect").attr("fill", "url(#grid)");

			// Function to update grid to cover exactly the viewport
			const updateGridCoverage = (transform: d3.ZoomTransform) => {
				// Convert viewport corners to container coordinate system
				const topLeft = transform.invert([0, 0]);
				const bottomRight = transform.invert([props.width, props.height]);

				// Set grid rectangle to cover exactly the viewport
				gridRect
					.attr("x", topLeft[0])
					.attr("y", topLeft[1])
					.attr("width", bottomRight[0] - topLeft[0])
					.attr("height", bottomRight[1] - topLeft[1]);
			};

			// Add zoom behavior
			const zoom = d3
				.zoom<SVGSVGElement, unknown>()
				.scaleExtent([0.1, 10])
				.on("zoom", (event) => {
					container.attr("transform", event.transform);

					// Update grid scale to maintain consistent visual size
					const scale = event.transform.k;
					const gridSize = 50 / scale;
					gridPattern.attr("width", gridSize).attr("height", gridSize);
					gridPattern
						.select("path")
						.attr("d", `M ${gridSize} 0 L 0 0 0 ${gridSize}`)
						.attr("stroke-width", 1 / scale);

					// Update grid coverage to match viewport
					updateGridCoverage(event.transform);
				});

			svg.call(zoom);

			// Initialize grid coverage
			updateGridCoverage(d3.zoomIdentity);

			const simulation = d3
				.forceSimulation(nodes)
				.force(
					"link",
					d3
						.forceLink(links)
						.id((d: any) => d.id)
						.distance(120)
				)
				.force("charge", d3.forceManyBody().strength(-400))
				.force("center", d3.forceCenter(props.width / 2, props.height / 2));

			const link = container
				.append("g")
				.selectAll("line")
				.data(links)
				.join("line")
				.attr("stroke", "#cccccc")
				.attr("stroke-width", 2)
				.attr("marker-end", "url(#arrow)");

			const nodeDragBehaviour = d3
				.drag<
					// @ts-expect-error
					DragEvent,
					d3.SimulationNodeDatum & {
						id: number;
						type: "parameter" | "const" | "operation";
						label: string;
					}
				>()
				.on("start", (event, d) => {
					if (!event.active) simulation.alphaTarget(0.3).restart();
					d.fx = d.x;
					d.fy = d.y;
				})
				.on("drag", (event, d) => {
					d.fx = event.x;
					d.fy = event.y;
				})
				.on("end", (event, d) => {
					if (!event.active) simulation.alphaTarget(0);
					d.fx = null;
					d.fy = null;
				});

			const node = container
				.append("g")
				.selectAll("g")
				.data(nodes)
				.join("g")
				.attr("cursor", "pointer")
				// @ts-expect-error
				.call(nodeDragBehaviour);

			node
				.append("circle")
				.attr("r", 25)
				.attr("fill", (d) => colors[d.type]);

			node
				.append("text")
				.attr("text-anchor", "middle")
				.attr("dy", "0.35em")
				.attr("fill", "white")
				.attr("font-size", "14px")
				.attr("font-weight", "bold")
				.attr("font-family", "Inter, sans-serif")
				.text((d) => d.label);

			simulation.on("tick", () => {
				link
					.attr("x1", (d) => (d.source as d3.SimulationNodeDatum).x || 0)
					.attr("y1", (d) => (d.source as d3.SimulationNodeDatum).y || 0)
					.attr("x2", (d) => (d.target as d3.SimulationNodeDatum).x || 0)
					.attr("y2", (d) => (d.target as d3.SimulationNodeDatum).y || 0);

				node.attr("transform", (d) => `translate(${d.x},${d.y})`);
			});
		};

		onMounted(() => {
			createVisualization();
		});

		watch(
			() => props.graph,
			() => {
				createVisualization();
			},
			{ deep: true }
		);

		return () => (
			<svg
				ref={svgRef}
				width={props.width}
				height={props.height}
				style={{ backgroundColor: "#1a1a1a" }}
			/>
		);
	},
});

export default defineComponent(() => {
	const graph: DFGNode[] = [
		{
			kind: {
				type: "Parameter",
				index: 0,
			},
			uses: [4],
		},
		{
			kind: {
				type: "Parameter",
				index: 1,
			},
			uses: [4],
		},
		{
			kind: {
				type: "Parameter",
				index: 2,
			},
			uses: [5],
		},
		{
			kind: {
				type: "Parameter",
				index: 3,
			},
			uses: [5],
		},
		{
			kind: {
				type: "Add",
				left: 0,
				right: 1,
			},
			uses: [6],
		},
		{
			kind: {
				type: "Mul",
				left: 2,
				right: 3,
			},
			uses: [6],
		},
		{
			kind: {
				type: "Sub",
				left: 4,
				right: 5,
			},
			uses: [8],
		},
		{
			kind: {
				type: "ConstInt",
				value: 2,
			},
			uses: [8],
		},
		{
			kind: {
				type: "Div",
				left: 6,
				right: 7,
			},
			uses: [10],
		},
		{
			kind: {
				type: "ConstInt",
				value: 42,
			},
			uses: [10],
		},
		{
			kind: {
				type: "Add",
				left: 8,
				right: 9,
			},
			uses: [],
		},
	];

	return () => (
		<DFGGraph
			graph={graph}
			width={window.innerWidth}
			height={window.innerHeight}
		/>
	);
});
