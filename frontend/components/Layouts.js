import React from "react";
import { EventEmitter } from "../utils/EventEmitter";
import Split from "react-split";

const Layouts = (props) => {
  const { splitType, children } = props;
  return (
    <>
      <Split
        className="split"
        minSize={0}
        snapOffset={0}
        expandToMin={false}
        gutterSize={10}
        gutterAlign="center"
        dragInterval={1}
        direction={splitType}
        style={{
          flexGrow: 1,
          ...(splitType === "horizontal"
            ? { display: "flex", flexDirection: "row", alignItems: "stretch" }
            : {}),
        }}
        onDrag={() => EventEmitter.dispatch("resize")}
      >
        {children}
      </Split>
    </>
  );
};

export default Layouts;
