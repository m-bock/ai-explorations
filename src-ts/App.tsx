import React, { useEffect, useMemo } from "react";
import * as stm from "../output/StateMachine";
import * as int from "../output/Data.Int";
import {
  ScatterChart,
  Scatter,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ReferenceLine
} from "recharts";
import type { Int } from "../output/Prim/index.js";
import type { Vec } from "../output/Data.Vector2/index.js";
import { Button } from "@/components/ui/button"
import "@/App.css"
import { Slider } from "@/components/ui/slider"

const App: React.FC = () => {
  const { state, dispatch } = stm.useStateMachine();

  useEffect(() => {
    dispatch.getDots();
  }, []);

  const all = stm.selAll(state);

  console.log("render")

  return (
    <div style={{ width: 320, height: 240 }}>
      <Chart data={state.dots} linePoints={all.linePoints} />
      {int.toNumber(state.epoch)}
      <div className="flex min-h-svh flex-col items-center justify-center">
        <Button variant={"destructive"}>Click me</Button>
        <Slider
          value={[int.toNumber(state.epoch)]}
          onValueChange={(values) => dispatch.msg(stm.mkMsg.SetEpoch(int.trunc(values[0] || 0)))} />
      </div>
    </div>
  );
};

interface ChartProps {
  data: { x: Int, y: Int }[];
  linePoints: { start: Vec<number>, end: Vec<number> };
}

const Chart = ({ data, linePoints }: ChartProps) => {
  return (
    <ScatterChart
      style={{ width: '100%', maxWidth: '700px', maxHeight: '70vh', aspectRatio: 1.618 }}
      responsive
      margin={{
        top: 20,
        right: 20,
        bottom: 10,
        left: 10,
      }}
    >
      <ReferenceLine
        stroke="green"
        strokeDasharray="3 3"
        segment={[
          stm.vecToRec(linePoints.start),
          stm.vecToRec(linePoints.end)
        ]}
      />
      <CartesianGrid strokeDasharray="3 3" />
      <XAxis dataKey="x" type="number" domain={[0, 1]} />
      <YAxis dataKey="y" type="number" width="auto" domain={[0, 1]} />
      <Tooltip cursor={{ strokeDasharray: '3 3' }} />
      <Legend />
      <Scatter data={data} fill="#8884d8" isAnimationActive />
    </ScatterChart>
  );
};

export default App;


// function clipLine(m: number, b: number, xMin: number, xMax: number, yMin: number, yMax: number) {
//   const y1 = m * xMin + b;
//   const y2 = m * xMax + b;

//   // If both are out of Y range on the same side → no visible segment
//   if (y1 < yMin && y2 < yMin) return null;
//   if (y1 > yMax && y2 > yMax) return null;

//   // Clamp to y-range
//   const c1 = Math.max(yMin, Math.min(yMax, y1));
//   const c2 = Math.max(yMin, Math.min(yMax, y2));

//   return [
//     { x: xMin, y: c1 },
//     { x: xMax, y: c2 },
//   ];
// }
