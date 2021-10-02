import SearchIcon from "@mui/icons-material/Search";
import { Autocomplete, Box, Typography } from "@mui/material";
import Head from "next/head";
import { useRouter } from "next/router";
import React, { useRef, useState } from "react";
import LanguageLink from "../components/LanguageLink";
import { Search, SearchIconWrapper, StyledInputBase } from "../components/UI";
import langs from "../static/data.json";

export default function Home() {
  const [selected, setSelected] = useState(null);
  const router = useRouter();
  const search = useRef();

  const moveToEditor = (link) => {
    router.push(`/editor/${link.id}`);
  };

  React.useEffect(() => {
    if (search.current) {
      search.current.focus();
    }
  }, []);

  return (
    <>
      <Head>
        <title>Riju</title>
        <meta
          name="description"
          content="Riju - fast playground for any language"
        />
        <link rel="icon" href="/favicon.ico" />
        <meta
          name="viewport"
          content="minimum-scale=1, initial-scale=1, width=device-width"
        />
      </Head>
      <Box component="main" sx={{ m: 3 }}>
        <Box
          sx={{
            flexGrow: 1,
            display: "flex",
            alignItems: "center",
            flexDirection: "column",
            width: "100%",
          }}
        >
          <Typography
            variant="h2"
            component="h2"
            sx={{ fontWeight: "bolder", color: "#000000" }}
          >
            Riju
          </Typography>
          <Typography variant="subtitle2" sx={{ color: "rgb(113,128,150)" }}>
            fast online playground for every programming language
          </Typography>
          <Box
            sx={{
              width: "70%",
              m: 3,
              position: "sticky",
              top: 0,
              zIndex: (theme) => theme.zIndex.appBar + 1,
              borderRadius: (theme) => theme.shape.borderRadius,
              backgroundColor: (theme) =>
                theme.palette.type === "dark"
                  ? theme.palette.common.black
                  : theme.palette.common.white,
            }}
            id="search"
          >
            <Search>
              <SearchIconWrapper>
                <SearchIcon fontSize="large" color="action" />
              </SearchIconWrapper>
              <Autocomplete
                sx={{ width: "100%" }}
                options={langs}
                getOptionLabel={(option) => option.name}
                value={selected}
                onChange={(event, newValue) => {
                  if (!newValue) return;
                  setSelected(newValue);
                  moveToEditor(newValue);
                }}
                renderInput={(params) => (
                  <StyledInputBase
                    inputRef={(e) => {
                      params.InputProps.ref(e);
                      search.current = e;
                      return e;
                    }}
                    placeholder="Search…"
                    inputProps={{
                      ...params.inputProps,
                      "aria-label": "search",
                    }}
                  />
                )}
              />
            </Search>
          </Box>
          <Box
            sx={{
              mt: 1,
              width: "100%",
              display: "flex",
              flexWrap: "wrap",
              gap: 1,
              justifyContent: "center",
            }}
          >
            {langs.map((link, i) => (
              <LanguageLink key={i} link={link} />
            ))}
          </Box>
        </Box>
      </Box>
    </>
  );
}
