import * as d3 from "d3";
import { defineComponent, ref, onMounted, watch, type PropType } from "vue";

type ValueType = "i32" | "i64" | "f32" | "f64";

type DFGNodeKind =
	| {
			kind: "Int";
			value: number;
			ty: ValueType;
	  }
	| {
			kind: "Float";
			value: number;
			ty: ValueType;
	  }
	| {
			kind: "Param";
			index: number;
			symbol: number;
			ty: ValueType;
	  }
	| {
			kind: "Add";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "Sub";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "Mul";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "Div";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "Phi";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "Lt";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "LtEq";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "Gt";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "GtEq";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "Eq";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "NotEq";
			left: number;
			right: number;
			ty: ValueType;
	  }
	| {
			kind: "LoopParam";
			block_index: number;
			before: number;
			after: number;
			ty: ValueType;
	  };

interface DFGNode {
	kind: DFGNodeKind;
	uses: number[];
}

const createD3Nodes = (graph: DFGNode[]) => {
	return graph.map((node, index) => {
		const { kind } = node;
		let type: "parameter" | "const" | "operation" | "phi" | "loop_param";
		let label: string;

		switch (kind.kind) {
			case "Int":
				type = "const";
				label = kind.value.toString();
				break;
			case "Float":
				type = "const";
				label = kind.value.toString();
				break;
			case "Param":
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
			case "Phi":
				type = "phi";
				label = "PHI";
				break;
			case "Lt":
				type = "operation";
				label = "LT";
				break;
			case "LtEq":
				type = "operation";
				label = "LTEQ";
				break;
			case "Gt":
				type = "operation";
				label = "GT";
				break;
			case "GtEq":
				type = "operation";
				label = "GTEQ";
				break;
			case "Eq":
				type = "operation";
				label = "EQ";
				break;
			case "NotEq":
				type = "operation";
				label = "NEQ";
				break;
			case "LoopParam":
				type = "loop_param";
				label = `LOOP_PARAM(${kind.block_index})`;
				break;
		}

		return {
			id: index,
			type,
			label,
		} as d3.SimulationNodeDatum & {
			id: number;
			type: "parameter" | "const" | "operation" | "phi" | "loop_param";
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
			kind.kind === "Add" ||
			kind.kind === "Sub" ||
			kind.kind === "Mul" ||
			kind.kind === "Div" ||
			kind.kind === "Phi" ||
			kind.kind === "Lt" ||
			kind.kind === "LtEq" ||
			kind.kind === "Gt" ||
			kind.kind === "GtEq" ||
			kind.kind === "Eq" ||
			kind.kind === "NotEq"
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
		} else if (kind.kind === "LoopParam") {
			links.push({
				source: kind.before,
				target: index,
				type: "operand",
			});
			links.push({
				source: kind.after,
				target: index,
				type: "operand",
			});
		}
	}

	return links;
};

const DataFlowGraph = defineComponent({
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
			phi: "#952ad5",
			loop_param: "#d52a7f",
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
				kind: "Param",
				index: 0,
				symbol: 2,
				ty: "i32",
			},
			uses: [4],
		},
		{
			kind: {
				kind: "Param",
				index: 1,
				symbol: 4,
				ty: "i32",
			},
			uses: [5],
		},
		{
			kind: {
				kind: "Int",
				value: 0,
				ty: "i32",
			},
			uses: [6, 7],
		},
		{
			kind: {
				kind: "Int",
				value: 1,
				ty: "i32",
			},
			uses: [11, 16, 8],
		},
		{
			kind: {
				kind: "LoopParam",
				block_index: 1,
				before: 0,
				after: 0,
				ty: "i32",
			},
			uses: [12],
		},
		{
			kind: {
				kind: "LoopParam",
				block_index: 1,
				before: 1,
				after: 1,
				ty: "i32",
			},
			uses: [13],
		},
		{
			kind: {
				kind: "LoopParam",
				block_index: 1,
				before: 2,
				after: 18,
				ty: "i32",
			},
			uses: [18, 23],
		},
		{
			kind: {
				kind: "LoopParam",
				block_index: 1,
				before: 2,
				after: 11,
				ty: "i32",
			},
			uses: [11],
		},
		{
			kind: {
				kind: "LoopParam",
				block_index: 1,
				before: 3,
				after: 19,
				ty: "i32",
			},
			uses: [19, 27],
		},
		{
			kind: {
				kind: "LoopParam",
				block_index: 1,
				before: 2,
				after: 2,
				ty: "i32",
			},
			uses: [],
		},
		{
			kind: {
				kind: "LoopParam",
				block_index: 1,
				before: 2,
				after: 17,
				ty: "i32",
			},
			uses: [],
		},
		{
			kind: {
				kind: "Add",
				left: 7,
				right: 3,
				ty: "i32",
			},
			uses: [12, 13, 15, 16, 19, 7],
		},
		{
			kind: {
				kind: "Gt",
				left: 11,
				right: 4,
				ty: "i32",
			},
			uses: [],
		},
		{
			kind: {
				kind: "Gt",
				left: 11,
				right: 5,
				ty: "i32",
			},
			uses: [],
		},
		{
			kind: {
				kind: "Int",
				value: 2,
				ty: "i32",
			},
			uses: [15, 26],
		},
		{
			kind: {
				kind: "Mul",
				left: 11,
				right: 14,
				ty: "i32",
			},
			uses: [17],
		},
		{
			kind: {
				kind: "Add",
				left: 11,
				right: 3,
				ty: "i32",
			},
			uses: [17],
		},
		{
			kind: {
				kind: "Phi",
				left: 15,
				right: 16,
				ty: "i32",
			},
			uses: [18, 10],
		},
		{
			kind: {
				kind: "Add",
				left: 6,
				right: 17,
				ty: "i32",
			},
			uses: [22, 6],
		},
		{
			kind: {
				kind: "Mul",
				left: 8,
				right: 11,
				ty: "i32",
			},
			uses: [21, 22, 8],
		},
		{
			kind: {
				kind: "Int",
				value: 1000,
				ty: "i32",
			},
			uses: [21],
		},
		{
			kind: {
				kind: "Gt",
				left: 19,
				right: 20,
				ty: "i32",
			},
			uses: [],
		},
		{
			kind: {
				kind: "Add",
				left: 18,
				right: 19,
				ty: "i32",
			},
			uses: [23],
		},
		{
			kind: {
				kind: "Phi",
				left: 6,
				right: 22,
				ty: "i32",
			},
			uses: [25, 26, 27],
		},
		{
			kind: {
				kind: "Int",
				value: 100,
				ty: "i32",
			},
			uses: [25],
		},
		{
			kind: {
				kind: "Gt",
				left: 23,
				right: 24,
				ty: "i32",
			},
			uses: [],
		},
		{
			kind: {
				kind: "Mul",
				left: 23,
				right: 14,
				ty: "i32",
			},
			uses: [28],
		},
		{
			kind: {
				kind: "Add",
				left: 23,
				right: 8,
				ty: "i32",
			},
			uses: [28],
		},
		{
			kind: {
				kind: "Phi",
				left: 26,
				right: 27,
				ty: "i32",
			},
			uses: [],
		},
	];

	return () => (
		<DataFlowGraph
			graph={graph}
			width={window.innerWidth}
			height={window.innerHeight}
		/>
	);
});
