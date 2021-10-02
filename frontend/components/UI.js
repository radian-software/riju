import { alpha, InputBase, styled } from "@mui/material";

export const Search = styled("div")(({ theme }) => ({
  borderRadius: theme.shape.borderRadius,
  backgroundColor: alpha(theme.palette.common.white, 0.15),
  "&:hover": {
    backgroundColor: alpha(theme.palette.common.white, 0.25),
  },
  width: "100%",
  [theme.breakpoints.up("sm")]: {
    width: "auto",
  },
  display: "flex",
  alignItems: "center",
  // boxShadow: theme.shadows[3],
  minHeight: 70,
  border: `1px solid`,
  borderColor: theme.palette.primary.main,
  boxShadow: `0 2px 5px ${alpha(theme.palette.primary.main, 0.5)}`,
}));

export const SearchIconWrapper = styled("div")(() => ({
  position: "absolute",
  pointerEvents: "none",
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  marginLeft: 16,
}));

export const StyledInputBase = styled(InputBase)(({ theme }) => ({
  color: "inherit",
  height: "100%",
  width: "100%",
  "& .MuiInputBase-input": {
    fontSize: 30,
    // minHeight: 70,
    // fontWeight: "bolder",
    // vertical padding + font size from searchIcon
    paddingLeft: 32 + 24,
    transition: theme.transitions.create("width"),
    width: "100%",
    [theme.breakpoints.up("md")]: {
      //   width: "20ch",
    },
  },
  flexGrow: 1,
}));
