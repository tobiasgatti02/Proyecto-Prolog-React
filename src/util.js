export function numberToColor(num) {
    switch (num) {
        case 2: return "#99FFCC";
        case 4: return "#99CCFF";
        case 8: return "#9999FF";
        case 16: return "#CC99FF";
        case 32: return "#FF99FF";
        case 64: return "#FF66B2";
        case 128: return "#FF9999";
        case 256: return "#FF00FA";
        case 512: return "#ff3333";
        case 1024: return "#99004C";
        case 2048: return "#0080FF";
        case 4096: return "#0000FF";
        case 8192: return "#4C0099";
        case 16384: return "#009900";
        case 32768: return "#666600";
        case 65536: return "#006666";
        
        default: return "black";
    }
}

export const equalPos = (posA, posB) => posA.toString() === posB.toString();

export const valueInPos = (pos, grid, numOfColumns) => {
    return grid[pos[0] * numOfColumns + pos[1]];
}

export const posInPath = (pos, path) => {
    return path.some(posI => equalPos(posI, pos));
}

export const connectionInPath = (posA, posB, path) => {
    return path.some((pos, i) => equalPos(pos, posA) && i + 1 < path.length && equalPos(path[i + 1], posB));
}

export const isAdyacent = (posA, posB) => {
    return !equalPos(posA, posB) && Math.abs(posA[0] - posB[0]) <= 1 && Math.abs(posA[1] - posB[1]) <= 1;
}

const smallerPow2GreaterOrEqualThan = (num) => {
    const log2num = Math.floor(Math.log2(num));
    return Math.pow(2, log2num) === num ? num : Math.pow(2, log2num + 1);
}


export const joinResult = (path, grid, numOfColumns) => smallerPow2GreaterOrEqualThan(path.reduce((result, pos) => result + valueInPos(pos, grid, numOfColumns), 0));