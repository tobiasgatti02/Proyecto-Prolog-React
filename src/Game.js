import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Square from './Square';
import { joinResult } from './util';




let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  const [isCoolingDown, setIsCoolingDown] = useState(false);


  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }






  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setPath(newPath);
    console.log(JSON.stringify(newPath));
  }

  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
    */
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }
  const handleClick = () => {
    // Si ya se está enfriando, no hacer nada
    if (isCoolingDown) {
      return;
    }
  
    const gridS = JSON.stringify(grid);
    const queryS = "booster(" + gridS + "," + numOfColumns + ", RGrids)";
    
    // Activar el enfriamiento
    setIsCoolingDown(true);
  
    pengine.query(queryS, (success, response) => {
      if (success) {
        animateEffect(response['RGrids']);
      }
  
      // Desactivar el enfriamiento después de 3 segundos
      setTimeout(() => {
        setIsCoolingDown(false);
      }, 3000);
    });
  };

  const movidaMaximaClick = () => {
    // Si ya se está enfriando, no hacer nada
    if (isCoolingDown) {
      return;
    }
  
    const gridS = JSON.stringify(grid);
    const queryS = "movidaMaxima(" + gridS + "," + numOfColumns + ", CaminoMaximo)";
    setWaiting(true);
    // Activar el enfriamiento
    setIsCoolingDown(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setPath(response['CaminoMaximo']);
        setWaiting(false);
      }
      // Desactivar el enfriamiento después de 3 segundos
      setTimeout(() => {
        setIsCoolingDown(false);
      }, 3000);
    });
  };

  const movidaMaximaAdyacenteClick = () => {
    // Si ya se está enfriando, no hacer nada
    if (isCoolingDown) {
      return;
    }
  
    const gridS = JSON.stringify(grid);
    const queryS = "movidaMaximaAdyacenteIgual(" + gridS + "," + numOfColumns + ", CaminoMaximo)";
    setWaiting(true);
    // Activar el enfriamiento
    setIsCoolingDown(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setPath(response['CaminoMaximo']);
        setWaiting(false);
      }
      // Desactivar el enfriamiento después de 3 segundos
      setTimeout(() => {
        setIsCoolingDown(false);
      }, 3000);
    });
  };
  

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 500);
    } else {
      setWaiting(false);
    }
  }

  
  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">
        {path.length > 0 ?
          <Square className={"nextSquare"} value={joinResult(path, grid, numOfColumns)} /> :
          <div className="score">
          <div className="score-text">score</div>
          <div className="score-value">{score}</div></div>}
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
      <button className={`boton-poderes ${isCoolingDown ? 'disabled-button' : ''}`} onClick={handleClick} disabled={isCoolingDown}>
        Colapsar Iguales
      </button>
      <button className={`boton-poderes ${isCoolingDown ? 'disabled-button' : ''}`} onClick={movidaMaximaClick} disabled={isCoolingDown}>
        Movida Máxima
      </button>
      <button className={`boton-poderes ${isCoolingDown ? 'disabled-button' : ''}`} onClick={movidaMaximaAdyacenteClick} disabled={isCoolingDown}>
        Movida Máxima adyacente igual
      </button>
    </div>
  );
}
export default Game;
