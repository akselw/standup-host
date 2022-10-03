
create table team
(
    id   uuid not null default uuid_generate_v4(),
    navn text not null,
    shortname text not null,
    constraint unik_shortname unique (shortname),
    primary key (id)
);

create table teammedlem
(
    id            uuid not null default uuid_generate_v4(),
    team_id uuid  not null references team,
    navn          text not null,
    primary key (id)
);

-- Security

alter table team
    enable row level security;

alter table teammedlem
    enable row level security;

create policy "Allow logged-in full access" on team for all using (auth.role() = 'authenticated');
create policy "Allow logged-in full access" on teammedlem for all using (auth.role() = 'authenticated');

create policy "Allow anon read access" on team for select using (auth.role() = 'anon');
create policy "Allow anon read access" on teammedlem for select using (auth.role() = 'anon');

-- Send "previous data" on change
alter table team replica identity full;
alter table teammedlem replica identity full;
