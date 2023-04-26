import React, { useState } from 'react';
import { numberToColor } from './util';
import clickSound from './click.mp3';

function Square({ value, onClick, onMouseEnter, className }) {
    const [audio] = useState(new Audio(clickSound));

    const handleClick = () => {
        audio.currentTime = 0;
        audio.play();
        onClick();
    };

    return (
        <div
            className={"square" + (className ? " " + className : "")}
            style={value === 0 ? undefined : { backgroundColor: numberToColor(value) }}
            onClick={handleClick} // Modificar esta línea
            onMouseEnter={onMouseEnter}
        >
            {value >= 16384 ? Math.trunc(value / 1000) + "k" : value === 0 ? "" : value}
        </div>
    );
}

export default Square;
