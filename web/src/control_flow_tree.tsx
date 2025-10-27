import { defineComponent, ref, onMounted } from "vue";
import * as d3 from "d3";

type DataResult =
	| {
			kind: "Unit";
	  }
	| {
			kind: "Never";
	  }
	| {
			kind: "Value";
			node_index: number;
	  };

type ControlNode =
	| {
			kind: "Loop";
			body: number;
			outputs: number[];
			result: DataResult;
	  }
	| {
			kind: "IfElse";
			condition: number;
			then_block: number;
			else_block: number | null;
			outputs: number[];
			result: DataResult;
	  }
	| {
			kind: "Return";
			value: DataResult;
	  }
	| {
			kind: "Break";
			target_block: number;
			value: DataResult;
	  }
	| {
			kind: "Continue";
			target_block: number;
	  };

interface Block {
	parent_index: number | null;
	statements: ControlNode[];
	result: DataResult;
}

const blocks: Block[] = [
	{
		parent_index: null,
		statements: [
			{
				kind: "Loop",
				body: 1,
				outputs: [6, 7, 8],
				result: {
					kind: "Value",
					node_index: 23,
				},
			},
			{
				kind: "IfElse",
				condition: 25,
				then_block: 6,
				else_block: 7,
				outputs: [],
				result: {
					kind: "Never",
				},
			},
		],
		result: {
			kind: "Value",
			node_index: 28,
		},
	},
	{
		parent_index: 0,
		statements: [
			{
				kind: "IfElse",
				condition: 12,
				then_block: 2,
				else_block: null,
				outputs: [],
				result: {
					kind: "Unit",
				},
			},
			{
				kind: "IfElse",
				condition: 13,
				then_block: 3,
				else_block: 4,
				outputs: [],
				result: {
					kind: "Value",
					node_index: 17,
				},
			},
			{
				kind: "IfElse",
				condition: 21,
				then_block: 5,
				else_block: null,
				outputs: [],
				result: {
					kind: "Unit",
				},
			},
		],
		result: {
			kind: "Value",
			node_index: 23,
		},
	},
	{
		parent_index: 1,
		statements: [
			{
				kind: "Break",
				target_block: 1,
				value: {
					kind: "Value",
					node_index: 6,
				},
			},
		],
		result: {
			kind: "Never",
		},
	},
	{
		parent_index: 1,
		statements: [],
		result: {
			kind: "Value",
			node_index: 15,
		},
	},
	{
		parent_index: 1,
		statements: [],
		result: {
			kind: "Value",
			node_index: 16,
		},
	},
	{
		parent_index: 1,
		statements: [
			{
				kind: "Break",
				target_block: 1,
				value: {
					kind: "Value",
					node_index: 22,
				},
			},
		],
		result: {
			kind: "Never",
		},
	},
	{
		parent_index: 0,
		statements: [
			{
				kind: "Return",
				value: {
					kind: "Value",
					node_index: 26,
				},
			},
		],
		result: {
			kind: "Never",
		},
	},
	{
		parent_index: 0,
		statements: [
			{
				kind: "Return",
				value: {
					kind: "Value",
					node_index: 27,
				},
			},
		],
		result: {
			kind: "Never",
		},
	},
];

interface TreeNode {
	id: number;
	label: string;
	children: TreeNode[];
	statements: string[];
	result: string;
}

function buildTree(blocks: Block[]): TreeNode {
	const createNode = (blockIndex: number): TreeNode => {
		const block = blocks[blockIndex];
		if (!block) {
			throw new Error(`Block ${blockIndex} not found`);
		}

		const statements = block.statements.map((stmt) => {
			switch (stmt.kind) {
				case "Loop":
					return `Loop → Block ${stmt.body}`;
				case "IfElse":
					return `If(node ${stmt.condition}) → ${stmt.then_block}${
						stmt.else_block !== null ? ` else ${stmt.else_block}` : ""
					}`;
				case "Return":
					return `Return ${
						stmt.value.kind === "Value"
							? `node ${stmt.value.node_index}`
							: stmt.value.kind
					}`;
				case "Break":
					return `Break → Block ${stmt.target_block}`;
				case "Continue":
					return `Continue → Block ${stmt.target_block}`;
			}
		});

		const resultStr =
			block.result.kind === "Value"
				? `Result: node ${block.result.node_index}`
				: `Result: ${block.result.kind}`;

		const children: TreeNode[] = [];
		for (const stmt of block.statements) {
			if (stmt.kind === "Loop") {
				children.push(createNode(stmt.body));
			} else if (stmt.kind === "IfElse") {
				children.push(createNode(stmt.then_block));
				if (stmt.else_block !== null) {
					children.push(createNode(stmt.else_block));
				}
			}
		}

		return {
			id: blockIndex,
			label: `Block ${blockIndex}`,
			children,
			statements,
			result: resultStr,
		};
	};

	return createNode(0);
}

const ControlFlowTree = defineComponent({
	name: "ControlFlowTree",
	setup() {
		const svgRef = ref<SVGSVGElement>();

		const renderTree = () => {
			if (!svgRef.value) return;

			const svg = d3.select(svgRef.value);
			const width = window.innerWidth;
			const height = window.innerHeight;

			svg.attr("width", width).attr("height", height);
			const container = svg.append("g");

			const zoom = d3
				.zoom<SVGSVGElement, unknown>()
				.scaleExtent([0.1, 10])
				.on("zoom", (event) => {
					container.attr("transform", event.transform);
				});

			svg.call(zoom);

			const g = container.append("g");

			const root = d3.hierarchy(buildTree(blocks));
			const treeLayout = d3.tree<TreeNode>().size([width - 200, height - 80]);

			treeLayout(root);

			// Draw links
			g.selectAll(".link")
				.data(root.links())
				.enter()
				.append("path")
				.attr("class", "link")
				.attr("fill", "none")
				.attr("stroke", "#555")
				.attr("stroke-width", 2)
				.attr("d", (d) => {
					const sx = d.source.x ?? 0;
					const sy = d.source.y ?? 0;
					const tx = d.target.x ?? 0;
					const ty = d.target.y ?? 0;
					return `M${sx},${sy}
                          C${sx},${(sy + ty) / 2}
                           ${tx},${(sy + ty) / 2}
                           ${tx},${ty}`;
				});

			// Draw nodes
			const nodes = g
				.selectAll(".node")
				.data(root.descendants())
				.enter()
				.append("g")
				.attr("class", "node")
				.attr("transform", (d) => `translate(${d.x},${d.y})`);

			nodes.each(function (d) {
				const node = d3.select(this);
				const texts: d3.Selection<SVGTextElement, unknown, null, undefined>[] =
					[];

				// Block label
				texts.push(
					node
						.append("text")
						.attr("dy", -5 - d.data.statements.length * 10)
						.attr("text-anchor", "middle")
						.style("font-weight", "bold")
						.style("font-size", "14px")
						.style("font-family", "Inter, sans-serif")
						.style("fill", "#ffffff")
						.text(d.data.label)
				);

				// Statements
				d.data.statements.forEach((stmt, i) => {
					texts.push(
						node
							.append("text")
							.attr("dy", 10 + i * 20 - d.data.statements.length * 10)
							.attr("text-anchor", "middle")
							.style("font-size", "11px")
							.style("font-family", "Inter, sans-serif")
							.style("fill", "#aaaaaa")
							.text(stmt)
					);
				});

				// Result
				texts.push(
					node
						.append("text")
						.attr("dy", 10 + d.data.statements.length * 10)
						.attr("text-anchor", "middle")
						.style("font-size", "11px")
						.style("font-family", "Inter, sans-serif")
						.style("fill", "#4ade80")
						.style("font-style", "italic")
						.text(d.data.result)
				);

				let maxWidth = 0;
				texts.forEach((text) => {
					const width = (text.node() as SVGTextElement).getBBox().width;
					if (width > maxWidth) {
						maxWidth = width;
					}
				});

				const padding = 20;
				const rectWidth = maxWidth + padding * 2;

				// Node background
				node
					.insert("rect", ":first-child")
					.attr("x", -rectWidth / 2)
					.attr("y", -10 - d.data.statements.length * 10 - 10)
					.attr("width", rectWidth)
					.attr("height", 20 + d.data.statements.length * 20 + 20)
					.attr("fill", "#2a2a2a")
					.attr("stroke", "#4a90e2")
					.attr("stroke-width", 2)
					.attr("rx", 5);
			});
		};

		onMounted(() => {
			renderTree();
		});

		return () => (
			<div
				style={{
					width: "100vw",
					height: "100vh",
					margin: 0,
					padding: 0,
					overflow: "hidden",
					backgroundColor: "#1a1a1a",
				}}
			>
				<svg ref={svgRef} style={{ width: "100%", height: "100%" }}></svg>
			</div>
		);
	},
});

export default ControlFlowTree;
