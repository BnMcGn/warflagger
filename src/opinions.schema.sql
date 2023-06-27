--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.25
-- Dumped by pg_dump version 9.5.25

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: author; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE public.author (
    id integer,
    type character varying(20),
    value text
);


ALTER TABLE public.author OWNER TO ben;

--
-- Name: author_id_seq; Type: SEQUENCE; Schema: public; Owner: ben
--

CREATE SEQUENCE public.author_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.author_id_seq OWNER TO ben;

--
-- Name: comment; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE public.comment (
    opinion integer NOT NULL,
    comment character varying
);


ALTER TABLE public.comment OWNER TO ben;

--
-- Name: comment_opinion_seq; Type: SEQUENCE; Schema: public; Owner: ben
--

CREATE SEQUENCE public.comment_opinion_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_opinion_seq OWNER TO ben;

--
-- Name: comment_opinion_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ben
--

ALTER SEQUENCE public.comment_opinion_seq OWNED BY public.comment.opinion;


--
-- Name: excerpt; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE public.excerpt (
    opinion integer,
    type character varying(20),
    value character varying
);


ALTER TABLE public.excerpt OWNER TO ben;

--
-- Name: looks; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE public.looks (
    firstlook timestamp without time zone NOT NULL,
    wf_user text NOT NULL,
    rootid integer NOT NULL,
    opinionid integer
);


ALTER TABLE public.looks OWNER TO ben;

--
-- Name: opinion; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE public.opinion (
    id integer NOT NULL,
    author integer NOT NULL,
    flag character varying(50) NOT NULL,
    votevalue integer NOT NULL,
    target text NOT NULL,
    datestamp timestamp without time zone NOT NULL,
    url text NOT NULL,
    rooturl integer NOT NULL,
    iid character varying(59)
);


ALTER TABLE public.opinion OWNER TO ben;

--
-- Name: opinion_id_seq; Type: SEQUENCE; Schema: public; Owner: ben
--

CREATE SEQUENCE public.opinion_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.opinion_id_seq OWNER TO ben;

--
-- Name: opinion_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ben
--

ALTER SEQUENCE public.opinion_id_seq OWNED BY public.opinion.id;


--
-- Name: reference; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE public.reference (
    opinion integer NOT NULL,
    reference text
);


ALTER TABLE public.reference OWNER TO ben;

--
-- Name: reference_opinion_seq; Type: SEQUENCE; Schema: public; Owner: ben
--

CREATE SEQUENCE public.reference_opinion_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.reference_opinion_seq OWNER TO ben;

--
-- Name: reference_opinion_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ben
--

ALTER SEQUENCE public.reference_opinion_seq OWNED BY public.reference.opinion;


--
-- Name: rooturl; Type: TABLE; Schema: public; Owner: ben
--

CREATE TABLE public.rooturl (
    id integer NOT NULL,
    rooturl text,
    rooturl_real boolean NOT NULL
);


ALTER TABLE public.rooturl OWNER TO ben;

--
-- Name: rooturl_id_seq; Type: SEQUENCE; Schema: public; Owner: ben
--

CREATE SEQUENCE public.rooturl_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.rooturl_id_seq OWNER TO ben;

--
-- Name: rooturl_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ben
--

ALTER SEQUENCE public.rooturl_id_seq OWNED BY public.rooturl.id;


--
-- Name: opinion; Type: DEFAULT; Schema: public; Owner: ben
--

ALTER TABLE ONLY public.comment ALTER COLUMN opinion SET DEFAULT nextval('public.comment_opinion_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ben
--

ALTER TABLE ONLY public.opinion ALTER COLUMN id SET DEFAULT nextval('public.opinion_id_seq'::regclass);


--
-- Name: opinion; Type: DEFAULT; Schema: public; Owner: ben
--

ALTER TABLE ONLY public.reference ALTER COLUMN opinion SET DEFAULT nextval('public.reference_opinion_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: ben
--

ALTER TABLE ONLY public.rooturl ALTER COLUMN id SET DEFAULT nextval('public.rooturl_id_seq'::regclass);


--
-- Name: comment_pkey; Type: CONSTRAINT; Schema: public; Owner: ben
--

ALTER TABLE ONLY public.comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (opinion);


--
-- Name: opinion_pkey; Type: CONSTRAINT; Schema: public; Owner: ben
--

ALTER TABLE ONLY public.opinion
    ADD CONSTRAINT opinion_pkey PRIMARY KEY (id);


--
-- Name: reference_pkey; Type: CONSTRAINT; Schema: public; Owner: ben
--

ALTER TABLE ONLY public.reference
    ADD CONSTRAINT reference_pkey PRIMARY KEY (opinion);


--
-- Name: rooturl_pkey; Type: CONSTRAINT; Schema: public; Owner: ben
--

ALTER TABLE ONLY public.rooturl
    ADD CONSTRAINT rooturl_pkey PRIMARY KEY (id);


--
-- Name: rooturl_unique_constraint; Type: CONSTRAINT; Schema: public; Owner: ben
--

ALTER TABLE ONLY public.rooturl
    ADD CONSTRAINT rooturl_unique_constraint UNIQUE (rooturl);


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- Name: TABLE author; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON TABLE public.author FROM PUBLIC;
REVOKE ALL ON TABLE public.author FROM ben;
GRANT ALL ON TABLE public.author TO ben;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.author TO warflagger;


--
-- Name: SEQUENCE author_id_seq; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON SEQUENCE public.author_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE public.author_id_seq FROM ben;
GRANT ALL ON SEQUENCE public.author_id_seq TO ben;
GRANT SELECT,UPDATE ON SEQUENCE public.author_id_seq TO warflagger;


--
-- Name: TABLE comment; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON TABLE public.comment FROM PUBLIC;
REVOKE ALL ON TABLE public.comment FROM ben;
GRANT ALL ON TABLE public.comment TO ben;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.comment TO warflagger;


--
-- Name: SEQUENCE comment_opinion_seq; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON SEQUENCE public.comment_opinion_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE public.comment_opinion_seq FROM ben;
GRANT ALL ON SEQUENCE public.comment_opinion_seq TO ben;
GRANT SELECT,UPDATE ON SEQUENCE public.comment_opinion_seq TO warflagger;


--
-- Name: TABLE excerpt; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON TABLE public.excerpt FROM PUBLIC;
REVOKE ALL ON TABLE public.excerpt FROM ben;
GRANT ALL ON TABLE public.excerpt TO ben;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.excerpt TO warflagger;


--
-- Name: TABLE looks; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON TABLE public.looks FROM PUBLIC;
REVOKE ALL ON TABLE public.looks FROM ben;
GRANT ALL ON TABLE public.looks TO ben;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.looks TO warflagger;


--
-- Name: TABLE opinion; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON TABLE public.opinion FROM PUBLIC;
REVOKE ALL ON TABLE public.opinion FROM ben;
GRANT ALL ON TABLE public.opinion TO ben;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.opinion TO warflagger;


--
-- Name: SEQUENCE opinion_id_seq; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON SEQUENCE public.opinion_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE public.opinion_id_seq FROM ben;
GRANT ALL ON SEQUENCE public.opinion_id_seq TO ben;
GRANT SELECT,UPDATE ON SEQUENCE public.opinion_id_seq TO warflagger;


--
-- Name: TABLE reference; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON TABLE public.reference FROM PUBLIC;
REVOKE ALL ON TABLE public.reference FROM ben;
GRANT ALL ON TABLE public.reference TO ben;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.reference TO warflagger;


--
-- Name: SEQUENCE reference_opinion_seq; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON SEQUENCE public.reference_opinion_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE public.reference_opinion_seq FROM ben;
GRANT ALL ON SEQUENCE public.reference_opinion_seq TO ben;
GRANT SELECT,UPDATE ON SEQUENCE public.reference_opinion_seq TO warflagger;


--
-- Name: TABLE rooturl; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON TABLE public.rooturl FROM PUBLIC;
REVOKE ALL ON TABLE public.rooturl FROM ben;
GRANT ALL ON TABLE public.rooturl TO ben;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.rooturl TO warflagger;


--
-- Name: SEQUENCE rooturl_id_seq; Type: ACL; Schema: public; Owner: ben
--

REVOKE ALL ON SEQUENCE public.rooturl_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE public.rooturl_id_seq FROM ben;
GRANT ALL ON SEQUENCE public.rooturl_id_seq TO ben;
GRANT SELECT,UPDATE ON SEQUENCE public.rooturl_id_seq TO warflagger;


--
-- Name: DEFAULT PRIVILEGES FOR SEQUENCES; Type: DEFAULT ACL; Schema: public; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON SEQUENCES  FROM PUBLIC;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON SEQUENCES  FROM postgres;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT,UPDATE ON SEQUENCES  TO warflagger;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: public; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON TABLES  FROM PUBLIC;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON TABLES  FROM postgres;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT,INSERT,DELETE,UPDATE ON TABLES  TO warflagger;


--
-- PostgreSQL database dump complete
--

