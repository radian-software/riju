import { createTheme } from "@mui/material/styles";

export const theme = createTheme({
  palette: {
    mode: "light",
    primary: {
      main: "#4d4dff",
    },
    background: {
      default: "white",
    },
    success: {
      main: "#48c78e",
    },
  },
  typography: {
    fontFamily: "Fira Code",
  },
  components: {
    MuiButton: {
      styleOverrides: {
        root: {
          textTransform: "none",
        },
        disableElevation: true,
      },
    },
  },
});
