import React from 'react';
import Plot from 'react-plotly.js';
import { useStateMachine } from '../../output/StateMachine/index.js';

const App = () => {
  //const { state, dispatch } = useStateMachine();
  return (
    <Plot
      data={[
        {
          x: [1, 2, 3],
          y: [2, 6, 3],
          type: 'scatter',
          mode: 'lines+markers',
          marker: { color: 'red' },
        },
        { type: 'bar', x: [1, 2, 3], y: [2, 5, 3] },
      ]}
      layout={{ width: 320, height: 240, title: { text: 'A Fancy Plot' } }}
    />
  );
};


export default App
