\restrict dbmate

-- Dumped from database version 16.14 (Ubuntu 16.14-0ubuntu0.24.04.1)
-- Dumped by pg_dump version 16.14 (Ubuntu 16.14-0ubuntu0.24.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: channels; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.channels (
    id integer NOT NULL,
    community_id integer NOT NULL,
    slug text NOT NULL,
    name text NOT NULL,
    topic text,
    "position" integer DEFAULT 0 NOT NULL,
    is_archived boolean DEFAULT false NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    indexable boolean DEFAULT true NOT NULL
);


--
-- Name: channels_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.channels_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: channels_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.channels_id_seq OWNED BY public.channels.id;


--
-- Name: chat_messages; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.chat_messages (
    id bigint NOT NULL,
    channel_id integer NOT NULL,
    user_id integer,
    content text NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    edited_at timestamp without time zone,
    deleted_at timestamp without time zone,
    search_tsv tsvector
);


--
-- Name: chat_messages_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.chat_messages_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: chat_messages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.chat_messages_id_seq OWNED BY public.chat_messages.id;


--
-- Name: comment_votes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.comment_votes (
    user_id integer NOT NULL,
    comment_id integer NOT NULL,
    direction integer NOT NULL
);


--
-- Name: comments; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.comments (
    id integer NOT NULL,
    content text NOT NULL,
    post_id integer NOT NULL,
    user_id integer NOT NULL,
    parent_id integer,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: comments_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.comments_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: comments_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.comments_id_seq OWNED BY public.comments.id;


--
-- Name: communities; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.communities (
    id integer NOT NULL,
    slug text NOT NULL,
    name text NOT NULL,
    description text,
    rules text,
    avatar_url text,
    banner_url text,
    allow_downvotes boolean DEFAULT true NOT NULL,
    sections_enabled boolean DEFAULT true NOT NULL,
    visibility text DEFAULT 'public'::text NOT NULL,
    indexable boolean DEFAULT true NOT NULL,
    is_network_community boolean DEFAULT false NOT NULL,
    onboarding_state text DEFAULT 'published'::text NOT NULL,
    discoverable boolean DEFAULT true NOT NULL,
    CONSTRAINT communities_onboarding_state_check CHECK ((onboarding_state = ANY (ARRAY['draft'::text, 'published'::text]))),
    CONSTRAINT communities_visibility_check CHECK ((visibility = ANY (ARRAY['public'::text, 'private'::text])))
);


--
-- Name: communities_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.communities_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: communities_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.communities_id_seq OWNED BY public.communities.id;


--
-- Name: community_bans; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_bans (
    user_id integer NOT NULL,
    community_id integer NOT NULL,
    banned_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP
);


--
-- Name: community_members; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_members (
    user_id integer NOT NULL,
    community_id integer NOT NULL
);


--
-- Name: community_moderators; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_moderators (
    user_id integer NOT NULL,
    community_id integer NOT NULL,
    promoted_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    role character varying(50) DEFAULT 'mod'::character varying NOT NULL
);


--
-- Name: community_sections; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_sections (
    id integer NOT NULL,
    community_id integer NOT NULL,
    name text NOT NULL,
    description text,
    "position" integer DEFAULT 0 NOT NULL,
    default_sort text DEFAULT 'new'::text NOT NULL,
    is_introduction_section boolean DEFAULT false NOT NULL,
    slug text DEFAULT ''::text NOT NULL,
    indexable boolean DEFAULT true NOT NULL
);


--
-- Name: community_sections_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.community_sections_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: community_sections_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.community_sections_id_seq OWNED BY public.community_sections.id;


--
-- Name: community_user_stats; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.community_user_stats (
    id integer NOT NULL,
    user_id integer NOT NULL,
    community_id integer NOT NULL,
    local_karma integer DEFAULT 0 NOT NULL,
    local_post_count integer DEFAULT 0 NOT NULL,
    local_comment_count integer DEFAULT 0 NOT NULL,
    first_active_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: community_user_stats_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.community_user_stats_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: community_user_stats_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.community_user_stats_id_seq OWNED BY public.community_user_stats.id;


--
-- Name: dream_session; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.dream_session (
    id text NOT NULL,
    label text NOT NULL,
    expires_at real NOT NULL,
    payload text NOT NULL
);


--
-- Name: github_installations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.github_installations (
    id bigint NOT NULL,
    github_installation_id bigint NOT NULL,
    github_account_id bigint NOT NULL,
    github_account_login text NOT NULL,
    github_account_type text NOT NULL,
    connected_by_user_id integer,
    status text DEFAULT 'active'::text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    revoked_at timestamp with time zone,
    CONSTRAINT github_installations_github_account_id_check CHECK ((github_account_id > 0)),
    CONSTRAINT github_installations_github_account_login_check CHECK ((btrim(github_account_login) <> ''::text)),
    CONSTRAINT github_installations_github_account_type_check CHECK ((github_account_type = ANY (ARRAY['user'::text, 'organization'::text]))),
    CONSTRAINT github_installations_github_installation_id_check CHECK ((github_installation_id > 0)),
    CONSTRAINT github_installations_revoked_at_check CHECK (((revoked_at IS NULL) OR (status = 'revoked'::text))),
    CONSTRAINT github_installations_status_check CHECK ((status = ANY (ARRAY['active'::text, 'revoked'::text, 'inaccessible'::text])))
);


--
-- Name: github_installations_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.github_installations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: github_installations_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.github_installations_id_seq OWNED BY public.github_installations.id;


--
-- Name: github_onboarding_states; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.github_onboarding_states (
    id bigint NOT NULL,
    state_hash text NOT NULL,
    user_id integer NOT NULL,
    session_binding_hash text NOT NULL,
    flow text NOT NULL,
    pending_github_installation_id bigint,
    expires_at timestamp with time zone NOT NULL,
    consumed_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT github_onboarding_states_consumed_after_created_check CHECK (((consumed_at IS NULL) OR (consumed_at >= created_at))),
    CONSTRAINT github_onboarding_states_expires_after_created_check CHECK ((expires_at > created_at)),
    CONSTRAINT github_onboarding_states_flow_check CHECK ((flow = 'project_onboarding'::text)),
    CONSTRAINT github_onboarding_states_pending_github_installation_id_check CHECK (((pending_github_installation_id IS NULL) OR (pending_github_installation_id > 0))),
    CONSTRAINT github_onboarding_states_session_binding_hash_check CHECK ((btrim(session_binding_hash) <> ''::text)),
    CONSTRAINT github_onboarding_states_state_hash_check CHECK ((btrim(state_hash) <> ''::text))
);


--
-- Name: github_onboarding_states_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.github_onboarding_states_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: github_onboarding_states_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.github_onboarding_states_id_seq OWNED BY public.github_onboarding_states.id;


--
-- Name: mod_actions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.mod_actions (
    id integer NOT NULL,
    community_id integer NOT NULL,
    moderator_id integer NOT NULL,
    action_type character varying(50) NOT NULL,
    target_id integer,
    reason text NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: mod_actions_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.mod_actions_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: mod_actions_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.mod_actions_id_seq OWNED BY public.mod_actions.id;


--
-- Name: notifications; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.notifications (
    id integer NOT NULL,
    user_id integer NOT NULL,
    post_id integer,
    message text NOT NULL,
    is_read boolean DEFAULT false NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    notif_type character varying(50) DEFAULT 'comment_reply'::character varying NOT NULL
);


--
-- Name: notifications_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.notifications_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: notifications_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.notifications_id_seq OWNED BY public.notifications.id;


--
-- Name: page_views; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.page_views (
    id integer NOT NULL,
    path text NOT NULL,
    referer text,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    session_hash text DEFAULT ''::text NOT NULL
);


--
-- Name: page_views_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.page_views_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: page_views_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.page_views_id_seq OWNED BY public.page_views.id;


--
-- Name: password_resets; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.password_resets (
    id integer NOT NULL,
    user_id integer NOT NULL,
    token text NOT NULL,
    expires_at timestamp with time zone NOT NULL
);


--
-- Name: password_resets_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.password_resets_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: password_resets_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.password_resets_id_seq OWNED BY public.password_resets.id;


--
-- Name: pending_signups; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pending_signups (
    id bigint NOT NULL,
    username text NOT NULL,
    email text NOT NULL,
    password_hash text NOT NULL,
    token_hash text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    consumed_at timestamp with time zone,
    ip_address text,
    user_agent text
);


--
-- Name: pending_signups_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.pending_signups_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: pending_signups_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.pending_signups_id_seq OWNED BY public.pending_signups.id;


--
-- Name: post_votes; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.post_votes (
    user_id integer NOT NULL,
    post_id integer NOT NULL,
    direction integer NOT NULL
);


--
-- Name: posthog_group_cleanup_jobs; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posthog_group_cleanup_jobs (
    id bigint NOT NULL,
    group_key text NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    last_attempt_at timestamp with time zone,
    completed_at timestamp with time zone,
    CONSTRAINT posthog_group_cleanup_jobs_attempts_check CHECK ((attempts >= 0)),
    CONSTRAINT posthog_group_cleanup_jobs_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'completed'::text])))
);


--
-- Name: posthog_group_cleanup_jobs_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.posthog_group_cleanup_jobs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: posthog_group_cleanup_jobs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.posthog_group_cleanup_jobs_id_seq OWNED BY public.posthog_group_cleanup_jobs.id;


--
-- Name: posthog_person_deletion_jobs; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posthog_person_deletion_jobs (
    id bigint NOT NULL,
    distinct_id text NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    last_attempt_at timestamp with time zone,
    completed_at timestamp with time zone,
    CONSTRAINT posthog_person_deletion_jobs_attempts_check CHECK ((attempts >= 0)),
    CONSTRAINT posthog_person_deletion_jobs_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'completed'::text])))
);


--
-- Name: posthog_person_deletion_jobs_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.posthog_person_deletion_jobs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: posthog_person_deletion_jobs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.posthog_person_deletion_jobs_id_seq OWNED BY public.posthog_person_deletion_jobs.id;


--
-- Name: posts; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.posts (
    id integer NOT NULL,
    title text NOT NULL,
    url text,
    content text,
    community_id integer NOT NULL,
    user_id integer NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    image_url text,
    section_id integer,
    last_activity_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    promoted_from_channel_id integer
);


--
-- Name: posts_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.posts_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: posts_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.posts_id_seq OWNED BY public.posts.id;


--
-- Name: rate_limits; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.rate_limits (
    ip_address text NOT NULL,
    endpoint text NOT NULL,
    attempts integer DEFAULT 1 NOT NULL,
    window_start real NOT NULL
);


--
-- Name: reports; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.reports (
    id integer NOT NULL,
    community_id integer NOT NULL,
    reporter_user_id integer NOT NULL,
    target_type text NOT NULL,
    target_id bigint NOT NULL,
    target_author_user_id integer,
    reason text NOT NULL,
    details text,
    status text DEFAULT 'open'::text NOT NULL,
    action_kind text,
    resolution_note text,
    resolved_by_user_id integer,
    resolved_at timestamp without time zone,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT reports_action_kind_check CHECK (((action_kind IS NULL) OR (action_kind = ANY (ARRAY['removed_content'::text, 'banned_author'::text, 'other'::text])))),
    CONSTRAINT reports_reason_check CHECK ((reason = ANY (ARRAY['spam'::text, 'abuse'::text, 'off_topic'::text, 'illegal'::text, 'other'::text]))),
    CONSTRAINT reports_status_check CHECK ((status = ANY (ARRAY['open'::text, 'dismissed'::text, 'action_taken'::text]))),
    CONSTRAINT reports_target_type_check CHECK ((target_type = ANY (ARRAY['post'::text, 'comment'::text, 'chat_message'::text])))
);


--
-- Name: reports_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.reports_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: reports_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.reports_id_seq OWNED BY public.reports.id;


--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schema_migrations (
    version character varying NOT NULL
);


--
-- Name: thread_source_messages; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.thread_source_messages (
    post_id integer NOT NULL,
    message_id bigint NOT NULL,
    "position" integer DEFAULT 0 NOT NULL,
    is_seed boolean DEFAULT false NOT NULL
);


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id integer NOT NULL,
    username text NOT NULL,
    email text NOT NULL,
    password_hash text NOT NULL,
    verification_token text,
    is_email_verified boolean DEFAULT false NOT NULL,
    is_admin boolean DEFAULT false NOT NULL,
    is_banned boolean DEFAULT false NOT NULL,
    bio text,
    avatar_url text,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_active_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: channels id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channels ALTER COLUMN id SET DEFAULT nextval('public.channels_id_seq'::regclass);


--
-- Name: chat_messages id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages ALTER COLUMN id SET DEFAULT nextval('public.chat_messages_id_seq'::regclass);


--
-- Name: comments id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments ALTER COLUMN id SET DEFAULT nextval('public.comments_id_seq'::regclass);


--
-- Name: communities id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.communities ALTER COLUMN id SET DEFAULT nextval('public.communities_id_seq'::regclass);


--
-- Name: community_sections id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_sections ALTER COLUMN id SET DEFAULT nextval('public.community_sections_id_seq'::regclass);


--
-- Name: community_user_stats id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_user_stats ALTER COLUMN id SET DEFAULT nextval('public.community_user_stats_id_seq'::regclass);


--
-- Name: github_installations id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_installations ALTER COLUMN id SET DEFAULT nextval('public.github_installations_id_seq'::regclass);


--
-- Name: github_onboarding_states id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_onboarding_states ALTER COLUMN id SET DEFAULT nextval('public.github_onboarding_states_id_seq'::regclass);


--
-- Name: mod_actions id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mod_actions ALTER COLUMN id SET DEFAULT nextval('public.mod_actions_id_seq'::regclass);


--
-- Name: notifications id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications ALTER COLUMN id SET DEFAULT nextval('public.notifications_id_seq'::regclass);


--
-- Name: page_views id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.page_views ALTER COLUMN id SET DEFAULT nextval('public.page_views_id_seq'::regclass);


--
-- Name: password_resets id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets ALTER COLUMN id SET DEFAULT nextval('public.password_resets_id_seq'::regclass);


--
-- Name: pending_signups id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_signups ALTER COLUMN id SET DEFAULT nextval('public.pending_signups_id_seq'::regclass);


--
-- Name: posthog_group_cleanup_jobs id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_group_cleanup_jobs ALTER COLUMN id SET DEFAULT nextval('public.posthog_group_cleanup_jobs_id_seq'::regclass);


--
-- Name: posthog_person_deletion_jobs id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_person_deletion_jobs ALTER COLUMN id SET DEFAULT nextval('public.posthog_person_deletion_jobs_id_seq'::regclass);


--
-- Name: posts id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts ALTER COLUMN id SET DEFAULT nextval('public.posts_id_seq'::regclass);


--
-- Name: reports id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports ALTER COLUMN id SET DEFAULT nextval('public.reports_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Name: channels channels_community_id_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT channels_community_id_slug_key UNIQUE (community_id, slug);


--
-- Name: channels channels_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT channels_pkey PRIMARY KEY (id);


--
-- Name: chat_messages chat_messages_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_pkey PRIMARY KEY (id);


--
-- Name: comment_votes comment_votes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comment_votes
    ADD CONSTRAINT comment_votes_pkey PRIMARY KEY (user_id, comment_id);


--
-- Name: comments comments_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_pkey PRIMARY KEY (id);


--
-- Name: communities communities_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.communities
    ADD CONSTRAINT communities_pkey PRIMARY KEY (id);


--
-- Name: communities communities_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.communities
    ADD CONSTRAINT communities_slug_key UNIQUE (slug);


--
-- Name: community_bans community_bans_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_bans
    ADD CONSTRAINT community_bans_pkey PRIMARY KEY (user_id, community_id);


--
-- Name: community_members community_members_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_members
    ADD CONSTRAINT community_members_pkey PRIMARY KEY (user_id, community_id);


--
-- Name: community_moderators community_moderators_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_moderators
    ADD CONSTRAINT community_moderators_pkey PRIMARY KEY (user_id, community_id);


--
-- Name: community_sections community_sections_community_id_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_sections
    ADD CONSTRAINT community_sections_community_id_slug_key UNIQUE (community_id, slug);


--
-- Name: community_sections community_sections_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_sections
    ADD CONSTRAINT community_sections_pkey PRIMARY KEY (id);


--
-- Name: community_user_stats community_user_stats_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_user_stats
    ADD CONSTRAINT community_user_stats_pkey PRIMARY KEY (id);


--
-- Name: community_user_stats community_user_stats_user_id_community_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_user_stats
    ADD CONSTRAINT community_user_stats_user_id_community_id_key UNIQUE (user_id, community_id);


--
-- Name: dream_session dream_session_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dream_session
    ADD CONSTRAINT dream_session_pkey PRIMARY KEY (id);


--
-- Name: github_installations github_installations_github_installation_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_installations
    ADD CONSTRAINT github_installations_github_installation_id_key UNIQUE (github_installation_id);


--
-- Name: github_installations github_installations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_installations
    ADD CONSTRAINT github_installations_pkey PRIMARY KEY (id);


--
-- Name: github_onboarding_states github_onboarding_states_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_onboarding_states
    ADD CONSTRAINT github_onboarding_states_pkey PRIMARY KEY (id);


--
-- Name: github_onboarding_states github_onboarding_states_state_hash_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_onboarding_states
    ADD CONSTRAINT github_onboarding_states_state_hash_key UNIQUE (state_hash);


--
-- Name: mod_actions mod_actions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mod_actions
    ADD CONSTRAINT mod_actions_pkey PRIMARY KEY (id);


--
-- Name: notifications notifications_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_pkey PRIMARY KEY (id);


--
-- Name: page_views page_views_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.page_views
    ADD CONSTRAINT page_views_pkey PRIMARY KEY (id);


--
-- Name: password_resets password_resets_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets
    ADD CONSTRAINT password_resets_pkey PRIMARY KEY (id);


--
-- Name: password_resets password_resets_token_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets
    ADD CONSTRAINT password_resets_token_key UNIQUE (token);


--
-- Name: pending_signups pending_signups_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_signups
    ADD CONSTRAINT pending_signups_pkey PRIMARY KEY (id);


--
-- Name: pending_signups pending_signups_token_hash_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pending_signups
    ADD CONSTRAINT pending_signups_token_hash_key UNIQUE (token_hash);


--
-- Name: post_votes post_votes_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.post_votes
    ADD CONSTRAINT post_votes_pkey PRIMARY KEY (user_id, post_id);


--
-- Name: posthog_group_cleanup_jobs posthog_group_cleanup_jobs_group_key_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_group_cleanup_jobs
    ADD CONSTRAINT posthog_group_cleanup_jobs_group_key_key UNIQUE (group_key);


--
-- Name: posthog_group_cleanup_jobs posthog_group_cleanup_jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_group_cleanup_jobs
    ADD CONSTRAINT posthog_group_cleanup_jobs_pkey PRIMARY KEY (id);


--
-- Name: posthog_person_deletion_jobs posthog_person_deletion_jobs_distinct_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_person_deletion_jobs
    ADD CONSTRAINT posthog_person_deletion_jobs_distinct_id_key UNIQUE (distinct_id);


--
-- Name: posthog_person_deletion_jobs posthog_person_deletion_jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posthog_person_deletion_jobs
    ADD CONSTRAINT posthog_person_deletion_jobs_pkey PRIMARY KEY (id);


--
-- Name: posts posts_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_pkey PRIMARY KEY (id);


--
-- Name: rate_limits rate_limits_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rate_limits
    ADD CONSTRAINT rate_limits_pkey PRIMARY KEY (ip_address, endpoint);


--
-- Name: reports reports_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_pkey PRIMARY KEY (id);


--
-- Name: schema_migrations schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schema_migrations
    ADD CONSTRAINT schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: thread_source_messages thread_source_messages_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.thread_source_messages
    ADD CONSTRAINT thread_source_messages_pkey PRIMARY KEY (post_id, message_id);


--
-- Name: users users_email_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_email_key UNIQUE (email);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users users_username_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: idx_chat_messages_channel_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_chat_messages_channel_id ON public.chat_messages USING btree (channel_id, id);


--
-- Name: idx_chat_messages_search_tsv; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_chat_messages_search_tsv ON public.chat_messages USING gin (search_tsv);


--
-- Name: idx_community_user_stats_community_karma; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_user_stats_community_karma ON public.community_user_stats USING btree (community_id, local_karma);


--
-- Name: idx_community_user_stats_community_post_count; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_community_user_stats_community_post_count ON public.community_user_stats USING btree (community_id, local_post_count);


--
-- Name: idx_github_onboarding_states_expires_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_github_onboarding_states_expires_at ON public.github_onboarding_states USING btree (expires_at);


--
-- Name: idx_github_onboarding_states_user_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_github_onboarding_states_user_id ON public.github_onboarding_states USING btree (user_id);


--
-- Name: idx_pending_signups_email_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_pending_signups_email_active ON public.pending_signups USING btree (lower(email)) WHERE (consumed_at IS NULL);


--
-- Name: idx_pending_signups_expires_at; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_pending_signups_expires_at ON public.pending_signups USING btree (expires_at);


--
-- Name: idx_pending_signups_username_active; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_pending_signups_username_active ON public.pending_signups USING btree (lower(username)) WHERE (consumed_at IS NULL);


--
-- Name: idx_posthog_deletion_jobs_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_posthog_deletion_jobs_pending ON public.posthog_person_deletion_jobs USING btree (created_at) WHERE (status = 'pending'::text);


--
-- Name: idx_posthog_group_cleanup_pending; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_posthog_group_cleanup_pending ON public.posthog_group_cleanup_jobs USING btree (created_at) WHERE (status = 'pending'::text);


--
-- Name: idx_reports_community_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_reports_community_status ON public.reports USING btree (community_id, status, created_at DESC);


--
-- Name: uniq_reports_open_per_reporter_target; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_reports_open_per_reporter_target ON public.reports USING btree (reporter_user_id, target_type, target_id) WHERE (status = 'open'::text);


--
-- Name: uniq_thread_source_seed_message; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uniq_thread_source_seed_message ON public.thread_source_messages USING btree (message_id) WHERE is_seed;


--
-- Name: channels channels_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.channels
    ADD CONSTRAINT channels_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: chat_messages chat_messages_channel_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_channel_id_fkey FOREIGN KEY (channel_id) REFERENCES public.channels(id) ON DELETE CASCADE;


--
-- Name: chat_messages chat_messages_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_messages
    ADD CONSTRAINT chat_messages_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: comment_votes comment_votes_comment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comment_votes
    ADD CONSTRAINT comment_votes_comment_id_fkey FOREIGN KEY (comment_id) REFERENCES public.comments(id) ON DELETE CASCADE;


--
-- Name: comment_votes comment_votes_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comment_votes
    ADD CONSTRAINT comment_votes_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: comments comments_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES public.comments(id) ON DELETE CASCADE;


--
-- Name: comments comments_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- Name: comments comments_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.comments
    ADD CONSTRAINT comments_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: community_bans community_bans_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_bans
    ADD CONSTRAINT community_bans_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_bans community_bans_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_bans
    ADD CONSTRAINT community_bans_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: community_members community_members_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_members
    ADD CONSTRAINT community_members_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_members community_members_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_members
    ADD CONSTRAINT community_members_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: community_moderators community_moderators_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_moderators
    ADD CONSTRAINT community_moderators_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_moderators community_moderators_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_moderators
    ADD CONSTRAINT community_moderators_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: community_sections community_sections_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_sections
    ADD CONSTRAINT community_sections_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_user_stats community_user_stats_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_user_stats
    ADD CONSTRAINT community_user_stats_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: community_user_stats community_user_stats_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.community_user_stats
    ADD CONSTRAINT community_user_stats_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: github_installations github_installations_connected_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_installations
    ADD CONSTRAINT github_installations_connected_by_user_id_fkey FOREIGN KEY (connected_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: github_onboarding_states github_onboarding_states_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.github_onboarding_states
    ADD CONSTRAINT github_onboarding_states_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: mod_actions mod_actions_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mod_actions
    ADD CONSTRAINT mod_actions_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: mod_actions mod_actions_moderator_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.mod_actions
    ADD CONSTRAINT mod_actions_moderator_id_fkey FOREIGN KEY (moderator_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: notifications notifications_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- Name: notifications notifications_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notifications
    ADD CONSTRAINT notifications_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: password_resets password_resets_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.password_resets
    ADD CONSTRAINT password_resets_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: post_votes post_votes_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.post_votes
    ADD CONSTRAINT post_votes_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- Name: post_votes post_votes_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.post_votes
    ADD CONSTRAINT post_votes_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: posts posts_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: posts posts_promoted_from_channel_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_promoted_from_channel_id_fkey FOREIGN KEY (promoted_from_channel_id) REFERENCES public.channels(id) ON DELETE SET NULL;


--
-- Name: posts posts_section_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_section_id_fkey FOREIGN KEY (section_id) REFERENCES public.community_sections(id) ON DELETE SET NULL;


--
-- Name: posts posts_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.posts
    ADD CONSTRAINT posts_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: reports reports_community_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_community_id_fkey FOREIGN KEY (community_id) REFERENCES public.communities(id) ON DELETE CASCADE;


--
-- Name: reports reports_reporter_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_reporter_user_id_fkey FOREIGN KEY (reporter_user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: reports reports_resolved_by_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_resolved_by_user_id_fkey FOREIGN KEY (resolved_by_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: reports reports_target_author_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.reports
    ADD CONSTRAINT reports_target_author_user_id_fkey FOREIGN KEY (target_author_user_id) REFERENCES public.users(id) ON DELETE SET NULL;


--
-- Name: thread_source_messages thread_source_messages_message_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.thread_source_messages
    ADD CONSTRAINT thread_source_messages_message_id_fkey FOREIGN KEY (message_id) REFERENCES public.chat_messages(id) ON DELETE CASCADE;


--
-- Name: thread_source_messages thread_source_messages_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.thread_source_messages
    ADD CONSTRAINT thread_source_messages_post_id_fkey FOREIGN KEY (post_id) REFERENCES public.posts(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

\unrestrict dbmate


--
-- Dbmate schema migrations
--

INSERT INTO public.schema_migrations (version) VALUES
    ('20260317133145'),
    ('20260317142357'),
    ('20260317170540'),
    ('20260318120000'),
    ('20260323140417'),
    ('20260402000000'),
    ('20260511150728'),
    ('20260511200000'),
    ('20260512100028'),
    ('20260608160856'),
    ('20260609105640'),
    ('20260616120000'),
    ('20260619120000'),
    ('20260620140000'),
    ('20260622120000'),
    ('20260722120000'),
    ('20260722200000'),
    ('20260723120000'),
    ('20260723150000');
