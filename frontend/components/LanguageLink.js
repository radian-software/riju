import { alpha, Button } from "@mui/material";
import { useRouter } from "next/router";
import React from "react";

const LanguageLink = (props) => {
  const { link } = props;
  const router = useRouter();

  const handleClick = () => {
    router.push(`/editor/${link.id}`);
  };

  return (
    <>
      <Button
        variant="contained"
        onClick={handleClick}
        disableElevation
        sx={{
          fontSize: 12,
          minHeight: 70,
          minWidth: "calc(calc(100vw - 120px) / 6)",
          color: (theme) => theme.palette.text.primary,
          background: (theme) => theme.palette.common.white,
          border: `1px solid rgb(226,232,240)`,
          ":hover": {
            background: (theme) => theme.palette.common.white,
            borderColor: (theme) => theme.palette.primary.main,
            color: (theme) => theme.palette.primary.main,
            boxShadow: (theme) =>
              `0 4px 4px ${alpha(theme.palette.primary.main, 0.25)}`,
          },
        }}
      >
        {link.name}
      </Button>
    </>
  );
};

export default LanguageLink;
